postgresql-pool
===============

[![CI Status](https://github.com/gvolpe/postgresql-pool/workflows/Haskell%20CI/badge.svg)](https://github.com/gvolpe/postgresql-pool/actions)

PostgreSQL single-connection pool with reconnection support, built on top of [PostgreSQL Simple](https://hackage.haskell.org/package/postgresql-simple).

```haskell
import           Database.Postgres.Pool
import qualified Database.PostgreSQL.Simple    as P

withConnectionPool connectInfo defaultSettings $ \pool ->
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
