{-# LANGUAGE FlexibleContexts, TemplateHaskell            #-}

module Common where 

import           Database.Persist.TH
import           Paths
import Data.Aeson.TH (deriveJSON, defaultOptions)
import           System.FilePath
import           Data.ConfigFile              as C
import           Data.Either.Utils
import           Control.Monad.Except
import qualified Data.Text              as T


data EndType = TimeOut | ENDE | Disconnect | Resign deriving (Show, Read, Eq)
derivePersistField "EndType"
deriveJSON defaultOptions ''EndType

data BS = JA | NEIN deriving (Show, Read, Eq)
derivePersistField "BS"
deriveJSON defaultOptions ''BS


for = flip map

lengthT :: [a] -> T.Text
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

  eitherConfig <- runExceptT $ do
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
