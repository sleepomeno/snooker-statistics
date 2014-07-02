{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Main where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans    (lift)
import           Data.Text              (pack, unpack)
import           Test.WebDriver

main :: IO ()
main = let conf = defaultCaps { browser = chrome } in
  void $ runSession defaultSession conf $
       do openPage "http://www.gamedesire.com"
          setImplicitWait 3000
          loginBtn <- findElem $ ByClass (pack "login_button")
          click loginBtn
          liftIO $ threadDelay 3500
          loginOverlay <- findElem $ ById (pack "overlay_login_box")
          name <- findElem $ ById (pack "userLogin")
          password <- findElem $ ById (pack "user_passwd")
          loginForm <- findElem $ ById (pack "loginForm")
          sendKeys (pack "username") name
          sendKeys (pack "password") password
          submit loginForm
          liftIO $ threadDelay 3500000
          openPage "http://www.gamedesire.com/#/?dd=1&n=0&mod_name=player_results&sub=1&view=edit&gg=103"
          liftIO $ threadDelay 6500000
          title <- getTitle
          liftIO . putStrLn $ unpack title
          return ()



