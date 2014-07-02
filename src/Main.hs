{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (void,join,liftM)
import Control.Monad.Error (runErrorT,ErrorT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans (lift)
import Data.ConfigFile
import Data.Either.Utils
import Data.Text (pack, unpack)
import Paths
import System.FilePath
import Test.WebDriver


main :: IO ()
main = let conf = defaultCaps { browser = chrome } in
  void $ do
  dataDir <- getStaticDir
  let configFile = dataDir </> "config.txt"

  eitherConfig <- runErrorT $ do
    parser <- join $ liftIO $ readfile emptyCP configFile
    loginUser <- get parser "DEFAULT" "loginuser"
    loginPwd <- get parser "DEFAULT" "loginpwd"
    return (loginUser, loginPwd) :: ErrorT CPError IO (String, String)

  let (loginUser, loginPwd) = forceEither eitherConfig

  runSession defaultSession conf $
       do openPage "http://www.gamedesire.com"

          setImplicitWait 3000
          loginBtn <- findElem $ ByClass (pack "login_button")
          click loginBtn
          liftIO $ threadDelay 3500
          _ <- findElem $ ById (pack "overlay_login_box")
          name <- findElem $ ById (pack "userLogin")
          password <- findElem $ ById (pack "user_passwd")
          loginForm <- findElem $ ById (pack "loginForm")
          sendKeys (pack loginUser) name
          sendKeys (pack loginPwd) password
          submit loginForm
          liftIO $ threadDelay 3500000
          openPage "http://www.gamedesire.com/#/?dd=1&n=0&mod_name=player_results&sub=1&view=edit&gg=103"
          liftIO $ threadDelay 6500000
          title <- getTitle
          liftIO . putStrLn $ unpack title
          return ()



