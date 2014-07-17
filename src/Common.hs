module Common (ResultLine(..), EndType, BS, Seconds) where 

import           Data.Time

data EndType = TimeOut | ENDE | Disconnect | Resign deriving (Show, Read)
data BS = JA | NEIN deriving (Show, Read)
type Seconds = Int
data ResultLine = ResultLine {date        :: UTCTime
                            , duration    :: Seconds
                            , player1     :: String
                            , player2     :: String
                            , winner      :: String
                            , ranking1    :: Double
                            , ranking2    :: Double
                            , difference1 :: Double
                            , difference2 :: Double
                            , ranked      :: BS
                            , endType     :: EndType
                            , maxBreak1   :: Int
                            , maxBreak2   :: Int
                           } deriving (Show)

