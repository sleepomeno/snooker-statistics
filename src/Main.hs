{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad (join, liftM, void, (>=>))
import           Control.Monad.Error (ErrorT, runErrorT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans (lift)
import           Data.ConfigFile
import           Data.Either.Utils
import           Data.String
import           Data.Text (pack, unpack)
import           Data.Text.Internal (showText)
import           Data.Text.Lazy hiding (pack, unpack,take)
import           Data.Time
import           Data.Time.Format
import           Paths
import           System.FilePath
import           System.IO.Unsafe (unsafePerformIO)
import           Test.WebDriver
import qualified Text.XML as X

import           Parse
import           Common

data Config = Conf { loginUser :: String
                   , loginPwd  :: String
                   }

gSession :: WD WDSession
gSession = createSession $ defaultCaps { browser = chrome }

readConfig :: IO Config
readConfig = do
  dataDir <- getStaticDir
  let configFile = dataDir </> "config.txt"
      readProp p  = get p "DEFAULT"

  eitherConfig <- runErrorT $ do
    parser <- join $ liftIO $ readfile emptyCP configFile
    user <- readProp parser "loginuser"
    pwd <- readProp parser "loginpwd"
    return (user, pwd)

  let (user, pwd) = forceEither eitherConfig
  return $ Conf user pwd

loginGamedesire :: String -> String -> WD ()
loginGamedesire user pwd = do
    openPage "http://www.gamedesire.com"
    setImplicitWait 3000
    loginBtn  <- findElem $ ByClass (pack "login_button")
    click loginBtn
    liftIO $ threadDelay 3500
    findElem $ ById (pack "overlay_login_box")
    name      <- findElem $ ById (pack "userLogin")
    password  <- findElem $ ById (pack "user_passwd")
    loginForm <- findElem $ ById (pack "loginForm")
    sendKeys (pack user) name
    sendKeys (pack pwd) password
    submit loginForm

main :: IO ()
main = let conf = defaultCaps { browser = chrome } in
  void $ do
  Conf user pwd <- readConfig

  -- runSession defaultSession conf $ loginGamedesire user pwd >> resultsSource >>= writeToFile
  aFewResults <- runSession defaultSession conf $ loginGamedesire user pwd >> resultsSource >>= parseResultsSource >>= return . take 5
  mapM_ print aFewResults

parseResultsSource = return . getResultLines . X.parseText_ X.def . fromStrict 

resultsSource = do
    liftIO $ threadDelay 3500000
    openPage "http://www.gamedesire.com/#/?dd=1&n=0&mod_name=player_results&sub=1&view=edit&gg=103"
    liftIO $ threadDelay 6500000
    getSource

writeToFile source = liftIO $ writeFile resultsFile $ unpack source

parseDoc :: IO X.Document
parseDoc = X.readFile X.def $ fromString resultsFile

resultLines = parseDoc >>= return . getResultLines

results :: IO String
results = readFile resultsFile

resultsFile =  "/home/greg/haskell/snooker-statistics/results"
