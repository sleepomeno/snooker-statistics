{-# LANGUAGE TemplateHaskell            #-}

module Common where 

import           Data.Time
import           Database.Persist.TH
import           Paths
import Data.Aeson.TH (deriveJSON, defaultOptions)
import           System.FilePath
import Database.Persist
import           Data.ConfigFile              as C
import Control.Applicative
import Database.Persist.Sqlite
import           Data.Either.Utils
import           Control.Monad.Error          (ErrorT, runErrorT)
import           Control.Monad                (join, liftM, void, (>=>))
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStdoutLoggingT, logDebugN, logInfoN, logWarnN, logErrorN)
import qualified Data.Text              as T


data EndType = TimeOut | ENDE | Disconnect | Resign deriving (Show, Read, Eq)
derivePersistField "EndType"
deriveJSON defaultOptions ''EndType

data BS = JA | NEIN deriving (Show, Read, Eq)
derivePersistField "BS"
deriveJSON defaultOptions ''BS


for = flip map

lengthT = T.pack . show . length

--------------------------------
-------- Configuration ---------
--------------------------------
data Config = Conf { loginUser :: T.Text
                   , loginPwd  :: T.Text
                   , players   :: [T.Text]
                   , lastMatchesFile   :: T.Text
                   , outputDir :: T.Text
                   , rivalries :: T.Text
                   , rivalUsers   :: [T.Text]
                   } deriving (Show, Read)

readConfig :: IO Config
readConfig = do
  dataDir <- getStaticDir
  putStrLn $ "Datadir: " ++ dataDir
  let configFile = dataDir </> "config.txt"
      readProp p  = C.get p "DEFAULT"

  eitherConfig <- runErrorT $ do
    parser <- join $ liftIO $ readfile emptyCP configFile
    user <- T.pack <$> readProp parser "loginuser"
    pwd <- T.pack <$> readProp parser "loginpwd"
    players <- (T.split (== ',') . T.pack) <$> readProp parser "users"
    lastMatchesFile <- readProp parser "lastmatchesfile"
    outputDir <- readProp parser "outputdir"
    rivalries <- readProp parser "rivalries"
    rivalUsers <- (T.split (== ',') . T.pack) <$> readProp parser "rivalusers"
    return (user, pwd, players, lastMatchesFile, outputDir, rivalries, rivalUsers)

  let (user, pwd, players, lastMatchesFile, outputDir, rivalries, rivalUsers) = forceEither eitherConfig
  return $ Conf user pwd players lastMatchesFile outputDir rivalries rivalUsers
