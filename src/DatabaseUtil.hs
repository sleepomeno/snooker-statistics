{-# LANGUAGE OverloadedStrings #-}

module DatabaseUtil where

import           Database.Persist.TH
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStdoutLoggingT, logDebugN, logInfoN, logWarnN, logErrorN)

import Model

withDB x = runSqlite' "snook.db" $ do
    runMigration migrateAll
    x
runSqlite' connstr  = runResourceT . runStdoutLoggingT . withSqliteConn connstr . runSqlConn
