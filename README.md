postgresql-resilient
====================

[![CI Status](https://github.com/gvolpe/postgresql-resilient/workflows/Haskell%20CI/badge.svg)](https://github.com/gvolpe/postgresql-resilient/actions)

PostgreSQL single-connection pool with automatic reconnection support, built on top of [PostgreSQL Simple](https://hackage.haskell.org/package/postgresql-simple).

```haskell
import           Database.Postgres.Resilient
import qualified Database.PostgreSQL.Simple    as P

withResilientConnection connectInfo defaultSettings $ \pool ->
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
  managed $ withResilientConnection connectInfo defaultSettings

main :: IO ()
main = with mkConnection $ \pool ->
  (conn :: P.Connection) <- getConnection pool
  doSomething conn
```
