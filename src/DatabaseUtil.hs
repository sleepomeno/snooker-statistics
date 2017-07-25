{-# LANGUAGE FlexibleContexts, TypeFamilies, OverloadedStrings #-}

module DatabaseUtil where

import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStdoutLoggingT)

import Model

withDB x = runSqlite' "snook.db" $ do
    runMigration migrateAll
    x
runSqlite' connstr  = runResourceT . runStdoutLoggingT . withSqliteConn connstr . runSqlConn
