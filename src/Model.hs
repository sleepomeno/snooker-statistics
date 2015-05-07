{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards               #-}
module Model where

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Time
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text
import Common


type Seconds = Int

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Match json
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
    deriving Show Eq

LastMatch json
    player Text
    date UTCTime
    UniquePlayer player
    deriving Show
|]

instance Ord Match where
  compare m1 m2 = if (m1 == m2 || m1 == switched m2) then EQ else GT
   where
    switched m@(Match{..}) = m { matchPlayer1 = matchPlayer2, matchPlayer2 = matchPlayer1, matchMaxBreak1 = matchMaxBreak2, matchMaxBreak2 = matchMaxBreak1, matchRanking1 = matchRanking2, matchRanking2 = matchRanking1, matchDifference1 = matchDifference2, matchDifference2 = matchDifference1}
