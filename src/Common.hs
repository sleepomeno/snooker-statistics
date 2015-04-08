{-# LANGUAGE TemplateHaskell            #-}

module Common where 

import           Data.Time
import           Database.Persist.TH
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStdoutLoggingT, logDebugN, logInfoN, logWarnN, logErrorN)
import qualified Data.Text              as T


data EndType = TimeOut | ENDE | Disconnect | Resign deriving (Show, Read)
derivePersistField "EndType"
deriveJSON defaultOptions ''EndType

data BS = JA | NEIN deriving (Show, Read)
derivePersistField "BS"
deriveJSON defaultOptions ''BS


for = flip map

lengthT = T.pack . show . length
