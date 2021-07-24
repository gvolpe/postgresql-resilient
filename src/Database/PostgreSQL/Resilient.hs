{-# LANGUAGE DeriveAnyClass, DerivingVia #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings, RankNTypes #-}

{-|
Module      : Database.PostgreSQL.Resilient
Description : PostgreSQL single-connection pool with automatic reconnection support, built on top of postgresql-simple.
License     : Apache-2.0
Maintainer  : volpegabriel@gmail.com
Stability   : experimental

The `withResilientConnection` function gives us a `ResilientConnection` from which we can always get a health connection, while automatic reconnection with retries and exponential back-offs are being handled in the background.

@
import           Database.PostgreSQL.Resilient
import qualified Database.PostgreSQL.Simple    as P

withResilientConnection defaultResilientSettings logHandler connectInfo $ \pool ->
  (conn :: P.Connection) <- getConnection pool
  res <- P.query_ conn "SELECT * FROM foo"
  putStrLn $ show res

logHandler :: String -> IO ()
logHandler = putStrLn

connectInfo :: P.ConnectInfo
connectInfo = P.ConnectInfo
  { P.connectHost     = "localhost"
  , P.connectPort     = 5432
  , P.connectUser     = "postgres"
  , P.connectPassword = ""
  , P.connectDatabase = "store"
  }

defaultResilientSettings :: ResilientSettings
defaultResilientSettings = ResilientSettings
  { healthCheckEvery     = 3
  , exponentialBackoffThreshold = 10
  }
@
-}
module Database.PostgreSQL.Resilient
  ( ResilientConnection(..)
  , ResilientSettings(..)
  , Seconds
  , withResilientConnection
  , defaultResilientSettings
  )
where

import           Control.Concurrent             ( forkIO
                                                , killThread
                                                , threadDelay
                                                )
import           Control.Concurrent.MVar
import           Control.Monad                  ( forever )
import           Control.Monad.Catch
import           Data.IORef
import           Data.Functor                   ( void )
import           Data.Maybe                     ( fromJust )
import qualified Database.PostgreSQL.Simple    as P
import           GHC.IO.Exception
import           Prelude                 hiding ( init )

data DBConnectionError = DBConnectionError deriving (Exception, Show)

{- | Single connection pool with built-in reconnection -}
data ResilientConnection m = ResilientConnection
  { getConnection :: m P.Connection -- ^ Get the latest healthy connection.
  }

type LogHandler = String -> IO ()

{- | Represents amount of seconds -}
newtype Seconds = Seconds Int
  deriving (Eq, Num, Ord, Show) via Int

{- | The resilient settings -}
data ResilientSettings = ResilientSettings
  { healthCheckEvery :: Seconds            -- ^ How often to check the connection status.
  , exponentialBackoffThreshold :: Seconds -- ^ After this threshold, stop the exponential back-off.
  } deriving Show

{- | Default resilient settings -}
defaultResilientSettings :: ResilientSettings
defaultResilientSettings =
  ResilientSettings { healthCheckEvery = 3, exponentialBackoffThreshold = 10 }

{- | Sleep for n amount of seconds -}
sleep :: Seconds -> IO ()
sleep (Seconds n) = threadDelay (n * 1000000)

healthCheck :: LogHandler -> P.Connection -> IO ()
healthCheck logger conn = do
  (res :: [P.Only String]) <- P.query_ conn "SELECT version();"
  logger $ show res

{- | Returns a `ResilientConnection` from which you can always acquire the latest connection available.
 -
 - Re-connections with configurable retries and exponential back-offs as well as closing the connection once done using it (guaranteed by `bracket`) are too handled by this function.
 - -}
withResilientConnection
  :: forall a
   . ResilientSettings
  -> LogHandler
  -> P.ConnectInfo
  -> (ResilientConnection IO -> IO a)
  -> IO a
withResilientConnection settings logger info f = do
  ((,) <$> newIORef Nothing <*> newEmptyMVar) >>= \(connRef, signal) ->
    let shutdown = readMVar signal >>= killThread -- ends keep-alive process
        pool     = ResilientConnection (fromJust <$> readIORef connRef)
        ka       = keepAlive (reconnect connRef) pool
        init     = acquire connRef >> ka >>= putMVar signal
    in  bracket (pool <$ init) (release shutdown) f
 where
  acquire ref = do
    logger "Connecting to PostgreSQL"
    conn <- P.connect info
    conn <$ atomicWriteIORef ref (Just conn)

  release shutdown pool = do
    logger "Closing PostgreSQL connection"
    conn <- getConnection pool
    P.close conn
    logger "Shutdown PostgreSQL re-connection process"
    shutdown

  clean conn = do
    logger "Closing no longer valid PostgreSQL connection"
    P.close conn

  reconnect ref n = catch (void $ acquire ref) $ \(e :: SomeException) ->
    logger (retries e) >> sleep n >> reconnect ref n'
   where
    retries e = show e <> "\n >>> Retrying in " <> show n <> " seconds."
    t  = exponentialBackoffThreshold settings
    n' = if n >= t then t else n * 2

  keepAlive rec pool = forkIO $ forever $ do
    sleep $ healthCheckEvery settings
    logger "Checking PostgreSQL connection status"
    conn <- getConnection pool
    catch
      (healthCheck logger conn)
      (\(e :: IOError) ->
        -- OtherError is thrown on every internal libpq error such as connection error
        if ioe_type e == ResourceVanished || ioe_type e == OtherError
          then clean conn >> rec 1
          else logger (show e) >> throwM DBConnectionError
      )
