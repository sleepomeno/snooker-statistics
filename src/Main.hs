{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (join, liftM, void)
import Control.Monad.Error (ErrorT, runErrorT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Data.ConfigFile
import Data.Either.Utils
import Data.Text (pack, unpack)
import Paths
import System.FilePath
import System.IO.Unsafe(unsafePerformIO)
import Test.WebDriver

data Config = Conf { loginUser :: String
                   , loginPwd :: String
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
    loginBtn <- findElem $ ByClass (pack "login_button")
    click loginBtn
    liftIO $ threadDelay 3500
    findElem $ ById (pack "overlay_login_box")
    name <- findElem $ ById (pack "userLogin")
    password <- findElem $ ById (pack "user_passwd")
    loginForm <- findElem $ ById (pack "loginForm")
    sendKeys (pack user) name
    sendKeys (pack pwd) password
    submit loginForm

main :: IO ()
main = let conf = defaultCaps { browser = chrome } in
  void $ do
  Conf user pwd <- readConfig

  runSession defaultSession conf $ loginGamedesire user pwd >> resultsSource

resultsSource :: WD ()
resultsSource = do
    liftIO $ threadDelay 3500000
    openPage "http://www.gamedesire.com/#/?dd=1&n=0&mod_name=player_results&sub=1&view=edit&gg=103"
    liftIO $ threadDelay 6500000
    source <- getSource
    liftIO $ writeFile resultsFile $ unpack source
    return ()

results :: IO String
results = readFile resultsFile

resultsFile =  "/home/greg/haskell/snooker-statistics/results"



