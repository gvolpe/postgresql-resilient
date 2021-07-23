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
