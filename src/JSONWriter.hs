{-# LANGUAGE OverloadedStrings #-}

module JSONWriter where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Model
import           Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad

import DatabaseUtil
import Database.Persist
import Data.Monoid ((<>))
import Control.Monad.Logger (logDebugN, logInfoN, logWarnN, logErrorN)

import Common


main :: IO ()
main = withDB $ do
  matches <- selectList [] [Desc MatchDate]
  logInfoN $ "Return " <> lengthT matches <> " matches"
  
  liftIO $ BL.putStrLn (encode matches)
