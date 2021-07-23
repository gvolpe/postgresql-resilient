module Database.PostgreSQL.Logger where

import           Data.Text                      ( Text
                                                , unpack
                                                )

class Logger m where
  logDebug :: Text -> m ()
  logInfo :: Text -> m ()
  logError :: Text -> m ()

instance Logger IO where
  logDebug = putStrLn . unpack
  logInfo  = putStrLn . unpack
  logError = putStrLn . unpack
