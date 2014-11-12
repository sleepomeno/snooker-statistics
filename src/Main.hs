{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad (join, liftM, void, (>=>))
import           Control.Monad.Error (ErrorT, runErrorT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans (lift)
import           Data.ConfigFile
import           Data.Either.Utils
import           Data.String
import qualified Data.Text as T
-- import           Data.Text.Internal (showText)
import qualified Data.Text.Lazy as TL
import           Data.Time
import           Data.Time.Format
import           Paths
import           System.FilePath
import           System.IO.Unsafe (unsafePerformIO)
import           Test.WebDriver
import qualified Text.XML as X
import           Data.Functor
import           Test.WebDriver.Classes (getSession)
import           Data.IORef
import           Text.XML.Cursor hiding (element)

import           Text.Show.Pretty (ppShow)
import           Common
import           Parse
--------------------------------
-------- Configuration ---------
--------------------------------
data Config = Conf { loginUser :: T.Text
                   , loginPwd  :: T.Text
                   } deriving (Show, Read)

readConfig :: IO Config
readConfig = do
  dataDir <- getStaticDir
  let configFile = dataDir </> "config.txt"
      readProp p  = get p "DEFAULT"

  eitherConfig <- runErrorT $ do
    parser <- join $ liftIO $ readfile emptyCP configFile
    user <- T.pack <$> readProp parser "loginuser"
    pwd <- T.pack <$> readProp parser "loginpwd"
    return (user, pwd)

  let (user, pwd) = forceEither eitherConfig
  return $ Conf user pwd

-------------------------------------------------------------
------------- Login with username and password --------------
-------------------------------------------------------------

loginGamedesire :: T.Text -> T.Text -> WD ()
loginGamedesire user pwd = do
    openPage "http://www.gamedesire.com"
    setImplicitWait 3000
    loginBtn  <- findElemByClass "login_button"
    click loginBtn
    liftIO $ threadDelay 3500
    findElem $ ById "overlay_login_box"
    name      <- findElemById"userLogin"
    password  <- findElemById "user_passwd"
    loginForm <- findElemById "loginForm"
    sendKeys user name
    sendKeys pwd password
    submit loginForm


main :: IO ()
main = let conf = defaultCaps { browser = chrome } in
  void $ do
  Conf user pwd <- readConfig

  -- runSession defaultSession conf $ loginGamedesire user pwd >> resultsSource >>= writeToFile
  aFewResults <- runSession defaultSession conf $
                 loginGamedesire user pwd >> waitFor 3500000 >> resultsSource >>= getDocument -- login and get a readable XML document
                 >>= return . getResultLines -- do the parsing of the source and create according data structures
                 >>= return . take 5
  mapM_ print aFewResults

-- |Transform the text argument into a XML-Conduit readable document
getDocument :: T.Text -> WD X.Document
getDocument = return . X.parseText_ X.def . TL.fromStrict 

-- |Return HTML source of results page
resultsSource = do
    openPage resultsURL
    waitFor 6500000
    getSource

resultsURL = "http://www.gamedesire.com/#/?dd=16&n=2&sub=1&player=Momsen76&view=player_results&gg=103"

---------------------------------------------------
----------- Some webdriver helper methods ---------
---------------------------------------------------

findElemById :: T.Text -> WD Element
findElemById = findElem . ById
findElemByClass = findElem . ByClass

waitFor :: Int -> WD ()
waitFor = liftIO . threadDelay

---------------------------------------------------
------------- Global Session handling -------------
---------------------------------------------------

-- Top-Level mutable state like in https://www.haskell.org/haskellwiki/Top_level_mutable_state
-- You need a very good reason to do that
globalSession :: IORef WDSession
{-# NOINLINE globalSession #-}
globalSession = unsafePerformIO (newIORef defaultSession)

-- |Creates a new browser session and puts it into the global variable globalSession
--  This is intended for interactive use only.
initGlobalSession = runWD defaultSession $
                      newSession >>=
                    \x -> liftIO $ writeIORef globalSession x
    where
      newSession :: WD WDSession
      newSession = createSession $ defaultCaps { browser = chrome }


-- |For use in interactive development to run the webdriver action with the global session
runGlobal action = readIORef globalSession >>= \session -> runWD session action


-----------------------
------ CheatSheet for interactive use -----
-----------------------
parseDoc :: IO X.Document
parseDoc = X.readFile X.def $ fromString resultsFile
resultsFile =  "/home/greg/haskell/snooker-statistics/results"
writeToFile source = liftIO $ writeFile resultsFile $ T.unpack source

withDoc f = do
  doc <- fromDocument  <$> parseDoc
  f doc

withDocR f = withDoc (return . f)

saveDoc :: WD ()
saveDoc = do
  source <- getSource
  writeToFile source

login = loginGamedesire (T.pack "testaccount12345") (T.pack "gregor") >> waitFor 5000000

toResults = openPage resultsURL >> waitFor 5500000


initResults = initGlobalSession >> (runGlobal $ login >> toResults >> saveDoc)

--- :set -XOverloadedStrings
--- initResults
--- withDocR $ length . \x -> x $// element "table"
