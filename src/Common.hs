{-# LANGUAGE TemplateHaskell            #-}

module Common (for, EndType, BS, Seconds) where 

import           Data.Time
import           Database.Persist.TH

data EndType = TimeOut | ENDE | Disconnect | Resign deriving (Show, Read)
derivePersistField "EndType"

data BS = JA | NEIN deriving (Show, Read)
derivePersistField "BS"
type Seconds = Int

for = flip map
-- data ResultLine = ResultLine {date        :: UTCTime
--                             , duration    :: Seconds
--                             , player1     :: String
--                             , player2     :: String
--                             , winner      :: String
--                             , ranking1    :: Double
--                             , ranking2    :: Double
--                             , difference1 :: Double
--                             , difference2 :: Double
--                             , ranked      :: BS
--                             , endType     :: EndType
--                             , maxBreak1   :: Int
--                             , maxBreak2   :: Int
--                            } deriving (Show)

