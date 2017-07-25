{-# LANGUAGE FlexibleContexts, TemplateHaskell            #-}
module JSONCommon where

import           Database.Persist.TH
import Data.Aeson.TH (deriveJSON, defaultOptions)

data EndType = TimeOut | ENDE | Disconnect | Resign deriving (Show, Read, Eq)
derivePersistField "EndType"
deriveJSON defaultOptions ''EndType

data BS = JA | NEIN deriving (Show, Read, Eq)
derivePersistField "BS"
deriveJSON defaultOptions ''BS

  
