{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Time
import Data.Text
import Common

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Match
    date UTCTime
    duration Int
    player1 Text
    player2 Text
    winner Text
    ranking1 Double
    ranking2 Double
    difference1 Double
    difference2 Double
    ranked BS
    endType EndType
    maxBreak1 Int
    maxBreak2 Int
    MatchHash duration player1 player2 winner ranking1 ranking2 maxBreak1 maxBreak2
    deriving Show

LastMatch
    player Text
    date UTCTime
    UniquePlayer player
    deriving Show
|]
