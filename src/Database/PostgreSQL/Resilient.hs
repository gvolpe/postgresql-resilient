{-# LANGUAGE DeriveAnyClass, DerivingVia #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

{- | Provides a way to acquire a Postgres connection pool with automatic reconnection. -}
module Database.PostgreSQL.Resilient
  ( ResilientConnection(..)
  , Seconds
  , withResilientConnection
  , defaultSettings
  )
where

import           Control.Concurrent             ( forkIO
                                                , killThread
                                                , threadDelay
                                                )
import           Control.Concurrent.MVar
import           Control.Monad                  ( forever )
import           Control.Monad.Catch
import           Database.PostgreSQL.Logger
import           Data.Functor                   ( void )
import           Data.IORef
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import qualified Database.PostgreSQL.Simple    as P
import           GHC.IO.Exception
import           Prelude                 hiding ( init )

data DBConnectionError = DBConnectionError deriving (Exception, Show)

{- | Single connection pool with built-in reconnection -}
data ResilientConnection m = ResilientConnection
  { getConnection :: m P.Connection
  , close :: m ()
  }

{- | Represents amount of seconds -}
newtype Seconds = Seconds Int
  deriving stock Show
  deriving Eq via Int
  deriving Ord via Int
  deriving Num via Int

data ReconnectSettings = ReconnectSettings
  { healthCheckEvery :: Seconds     -- How often to check the connection status.
  , exponentialThreshold :: Seconds -- After this threshold, stop the exponential back-off.
  } deriving Show

defaultSettings :: ReconnectSettings
defaultSettings =
  ReconnectSettings { healthCheckEvery = 3, exponentialThreshold = 10 }

{- | Sleep for n amount of seconds -}
sleep :: Seconds -> IO ()
sleep (Seconds n) = threadDelay (n * 1000000)

healthCheck :: P.Connection -> IO ()
healthCheck conn = do
  (res :: [P.Only String]) <- P.query_ conn "SELECT version();"
  logInfo $ T.pack (show res)

withResilientConnection
  :: forall a
   . ReconnectSettings
  -> P.ConnectInfo
  -> (ResilientConnection IO -> IO a)
  -> IO a
withResilientConnection settings info f = do
  connRef <- newIORef Nothing
  signal  <- newEmptyMVar
  let pool = ResilientConnection
        { getConnection = fromMaybe (error "Internal error")
                            <$> readIORef connRef
        , close         = readMVar signal >>= killThread
        }
  let init = acquire connRef >> keepAlive connRef pool >>= putMVar signal
  bracket (pool <$ init) release f
 where
  acquire ref = do
    logInfo "Acquiring PostgreSQL connection"
    conn <- P.connect info
    conn <$ atomicWriteIORef ref (Just conn)

  release pool = do
    logInfo "Releasing PostgreSQL connection"
    conn <- getConnection pool
    P.close conn
    logInfo "Closing PostgreSQL connection pool"
    close pool

  clean conn = do
    logDebug "Disposing of disconnected PostgreSQL connection"
    P.close conn

  keepAlive ref pool = forkIO $ forever $ do
    sleep $ healthCheckEvery settings
    logDebug "Checking PostgreSQL connection status"
    let reconnect n = do
          let t  = exponentialThreshold settings
          let n' = if n >= t then t else n * 2
          catch (void $ acquire ref) $ \(e :: SomeException) ->
            logError (T.pack $ retries e) >> sleep n >> reconnect n'
         where
          retries e = show e <> "\n > Retrying in " <> show n <> " seconds."
    conn <- getConnection pool
    catch
      (healthCheck conn)
      (\(e :: IOError) ->
        -- OtherError is thrown on every internal libpq error such as connection error
        if ioe_type e == ResourceVanished || ioe_type e == OtherError
          then clean conn >> reconnect 1
          else logError (T.pack $ show e) >> throwM DBConnectionError
      )
