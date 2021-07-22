{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingVia #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

{- | Provides a way to acquire a Postgres connection pool with automatic reconnection. -}
module Postgres.Pool
  ( ConnectionPool(..)
  , Seconds
  , seconds
  , withConnectionPool
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
import           Data.Functor                   ( void )
import           Data.IORef
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Text                     as T
import qualified Database.PostgreSQL.Simple    as P
import           GHC.IO.Exception
import           Postgres.DBLogger
import           Prelude                 hiding ( init )

data DBConnectionError = DBConnectionError deriving (Exception, Show)

{- | Single connection pool with built-in reconnection -}
data ConnectionPool m = ConnectionPool
  { getConnection :: m P.Connection
  , close :: m ()
  }

{- | Represents amount of seconds -}
newtype Seconds = Seconds Int
  deriving stock Show
  deriving Eq via Int
  deriving Ord via Int
  deriving Num via Int

seconds :: Int -> Maybe Seconds
seconds n | n > 0     = Just $ Seconds n
          | otherwise = Nothing

data ReconnectSettings = ReconnectSettings
  { healthCheckEvery :: Seconds     -- How often to check the connection status.
  , exponentialThreshold :: Seconds -- After this threshold, stop the exponential back-off.
  } deriving Show

defaultSettings :: ReconnectSettings
defaultSettings = ReconnectSettings { healthCheckEvery     = Seconds 3
                                    , exponentialThreshold = Seconds 10
                                    }

{- | Sleep for n amount of seconds -}
sleep :: Seconds -> IO ()
sleep (Seconds n) = threadDelay (n * 1000000)

healthCheck :: P.Connection -> IO ()
healthCheck conn = do
  (res :: [P.Only String]) <- P.query_ conn "SELECT version();"
  logInfo $ T.pack (show res)

withConnectionPool
  :: forall a
   . ReconnectSettings
  -> P.ConnectInfo
  -> (ConnectionPool IO -> IO a)
  -> IO a
withConnectionPool settings info f = do
  connRef <- newIORef Nothing
  signal  <- newEmptyMVar
  let pool = ConnectionPool
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
    clean conn
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
