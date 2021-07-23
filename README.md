postgresql-resilient
====================

[![CI Status](https://github.com/gvolpe/postgresql-resilient/workflows/Haskell%20CI/badge.svg)](https://github.com/gvolpe/postgresql-resilient/actions)

PostgreSQL single-connection pool with automatic reconnection support, built on top of [PostgreSQL Simple](https://hackage.haskell.org/package/postgresql-simple).

### Motivation

The `postgresql-simple` package gives us the two following functions (see [connection management](https://hackage.haskell.org/package/postgresql-simple-0.6.4/docs/Database-PostgreSQL-Simple.html#g:14)).

```haskell
connect :: ConnectInfo -> IO Connection
close :: Connection -> IO ()
```

Once we acquire a `Connection`, it would work as long as there are no connectivity issues. However, if the PostgreSQL becomes unreachable even for a second, such `Connection` will no longer be valid and you will need to `close` it and try to `connect` again, which could also fail if the server is still down.

So the tiny `postgresql-resilient` package provides a `ResilientConnection` from which we can always acquire the latest connection available. Re-connections with configurable retries and exponential back-offs as well as closing the connection once done using it (guaranteed by `bracket`) are handled for you too.

Therefore, instead of using `connect` directly, you can leverage the following function.

```haskell
withResilientConnection
  :: forall a
   . ReconnectSettings
  -> P.ConnectInfo
  -> (ResilientConnection IO -> IO a)
  -> IO a
```

It's also worth mentioning that it only depends on the `exceptions` and `text` packages, in addition to `postgresql-simple`.

### Quick Start

```haskell
import           Database.PostgreSQL.Resilient
import qualified Database.PostgreSQL.Simple    as P

withResilientConnection defaultSettings connectInfo $ \pool ->
  (conn :: P.Connection) <- getConnection pool
  doSomething conn

connectInfo :: P.ConnectInfo
connectInfo = P.ConnectInfo
  { P.connectHost     = "localhost"
  , P.connectPort     = 5432
  , P.connectUser     = "postgres"
  , P.connectPassword = ""
  , P.connectDatabase = "store"
  }

defaultSettings :: ReconnectSettings
defaultSettings = ReconnectSettings
  { healthCheckEvery     = 3
  , exponentialThreshold = 10
  }
```

E.g. using the [managed](https://hackage.haskell.org/package/managed) library for your resources.

```haskell
import           Control.Monad.Managed

mkConnection :: Managed (ResilientConnection IO)
mkConnection =
  managed $ withResilientConnection defaultSettings connectInfo

main :: IO ()
main = with mkConnection $ \pool ->
  (conn :: P.Connection) <- getConnection pool
  doSomething conn
```

#### Reconnection

Here are the logs of a simple connection example where the PostgreSQL server is shutdown on purpose and it's then brought back up a few seconds later.

```
$ cabal new-run postgresql-resilient-demo
Up to date
Acquiring PostgreSQL connection

Checking PostgreSQL connection status
[Only {fromOnly = "PostgreSQL 13.0 on x86_64-pc-linux-musl, compiled by gcc (Alpine 9.3.0) 9.3.0, 64-bit"}]

Checking PostgreSQL connection status
[Only {fromOnly = "PostgreSQL 13.0 on x86_64-pc-linux-musl, compiled by gcc (Alpine 9.3.0) 9.3.0, 64-bit"}]

Checking PostgreSQL connection status
Disposing of disconnected PostgreSQL connection
Acquiring PostgreSQL connection
libpq: failed (could not connect to server: Connection refused
	Is the server running on host "localhost" (::1) and accepting
	TCP/IP connections on port 5432?
could not connect to server: Connection refused
	Is the server running on host "localhost" (127.0.0.1) and accepting
	TCP/IP connections on port 5432?
)
 > Retrying in Seconds 1 seconds.

Acquiring PostgreSQL connection
libpq: failed (could not connect to server: Connection refused
	Is the server running on host "localhost" (::1) and accepting
	TCP/IP connections on port 5432?
could not connect to server: Connection refused
	Is the server running on host "localhost" (127.0.0.1) and accepting
	TCP/IP connections on port 5432?
)
 > Retrying in Seconds 2 seconds.

Acquiring PostgreSQL connection
libpq: failed (could not connect to server: Connection refused
	Is the server running on host "localhost" (::1) and accepting
	TCP/IP connections on port 5432?
could not connect to server: Connection refused
	Is the server running on host "localhost" (127.0.0.1) and accepting
	TCP/IP connections on port 5432?
)
 > Retrying in Seconds 4 seconds.

Acquiring PostgreSQL connection
Checking PostgreSQL connection status
[Only {fromOnly = "PostgreSQL 13.0 on x86_64-pc-linux-musl, compiled by gcc (Alpine 9.3.0) 9.3.0, 64-bit"}]

Checking PostgreSQL connection status
[Only {fromOnly = "PostgreSQL 13.0 on x86_64-pc-linux-musl, compiled by gcc (Alpine 9.3.0) 9.3.0, 64-bit"}]

^CReleasing PostgreSQL connection
Closing PostgreSQL connection pool
```

The health check is performed every 3 seconds by default but it is configurable via the `healthCheckEvery` setting. The retries are exponential by `^2` seconds with a threshold of 10 seconds, also configurable via `exponentialThreshold`.
