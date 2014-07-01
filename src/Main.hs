{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Main where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans    (lift)
import           Data.Text              (unpack)
import           Test.WebDriver

main :: IO ()
main = let conf = defaultCaps { browser = chrome } in
  void $ runSession defaultSession conf $
       do openPage "http://orf.at"
          title <- getTitle
          liftIO . putStrLn $ unpack title
          return ()



