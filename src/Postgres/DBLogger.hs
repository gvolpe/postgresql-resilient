module Postgres.DBLogger where

import           Data.Text                      ( Text
                                                , unpack
                                                )

class DBLogger m where
  logDebug :: Text -> m ()
  logInfo :: Text -> m ()
  logError :: Text -> m ()

instance DBLogger IO where
  logDebug = putStrLn . unpack
  logInfo  = putStrLn . unpack
  logError = putStrLn . unpack
