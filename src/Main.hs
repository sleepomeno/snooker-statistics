{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Main where

import Prelude hiding (log)
import           Control.Concurrent     (threadDelay)
import           Control.Monad          (join, liftM, void, (>=>))
import           Control.Monad.Error    (ErrorT, runErrorT)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans    (lift)
import           Control.Monad.Trans.Maybe
import Control.Error
import Data.Monoid ((<>))
import           Control.Monad.Cont
import           Data.ConfigFile as C
import           Data.Either.Utils
import           Data.String
import Data.Maybe
import qualified Data.Text              as T
-- import           Data.Text.Internal (showText)
import           Data.Functor
import           Data.IORef
import qualified Data.Text.Lazy         as TL
import           Data.Time
import           Data.Time.Format
import           Paths
import           System.FilePath
import           System.IO.Unsafe       (unsafePerformIO)
import           Test.WebDriver
import           Test.WebDriver.Classes (getSession)
import qualified Text.XML               as X
import           Text.XML.Cursor        hiding (element)

import           Common
import           Model
import           Parse
import           Text.Show.Pretty       (ppShow)

import Database.Persist
import Database.Persist.Sqlite
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStdoutLoggingT, logDebugN, logInfoN, logWarnN, logErrorN)


--------------------------------
-------- Configuration ---------
--------------------------------
data Config = Conf { loginUser :: T.Text
                   , loginPwd  :: T.Text
                   , players   :: [T.Text]
                   } deriving (Show, Read)

readConfig :: IO Config
readConfig = do
  dataDir <- getStaticDir
  let configFile = dataDir </> "config.txt"
      readProp p  = C.get p "DEFAULT"

  eitherConfig <- runErrorT $ do
    parser <- join $ liftIO $ readfile emptyCP configFile
    user <- T.pack <$> readProp parser "loginuser"
    pwd <- T.pack <$> readProp parser "loginpwd"
    players <- (T.split (== ',') . T.pack) <$> readProp parser "users"
    return (user, pwd, players)

  let (user, pwd, players) = forceEither eitherConfig
  return $ Conf user pwd players

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
type URL = String

-- l = lift . lift . lift


           
-- withDB x = runSqlite "snook.db" $ do 
--     runMigration migrateAll
--     x
-- withDB x = runStdoutLoggingT $ withSqlitePool ":memory:" 1  $ \pool -> runResourceT $ flip runSqlPool pool $ do 
withDB x = runSqlite' "snook.db" $ do
    runMigration migrateAll
    x

runSqlite' connstr  = runResourceT . runStdoutLoggingT . withSqliteConn connstr . runSqlConn

run session = runWD session
    
main :: IO ()
main = withDB $ do
  let conf = defaultCaps { browser = chrome } 
  Conf user pwd players <- liftIO readConfig

  let playerURLs = take 3 $ matchURLs $ head players
      stop = const False

  sess <- liftIO $ runWD defaultSession $ createSession conf
  let run' = runWD sess

  -- runSession defaultSession conf $ loginGamedesire user pwd >> resultsSource >>= writeToFile
  let init = liftIO $ run' $ do
        loginGamedesire user pwd
        waitFor 3500000
      fetchResults url = liftIO $ run' $ do 
        source <- resultsSource url
        doc <- getDocument source  
        return . getResultLines $ doc 


  -- runSession defaultSession conf $ do 
  init
  forM_ players $ \player -> do 
      maybeLastMatch <- getBy $ UniquePlayer player
      results <- (`runContT` return) $ callCC $ \ret -> do
          let getResults matches url = do 
                  results <- fetchResults (T.unpack url)
                  let tooOld = fromMaybe False tooOld'
                          where
                          tooOld' :: Maybe Bool
                          tooOld' = do
                              (Entity _ (LastMatch _ date')) <- maybeLastMatch
                              match <- headMay results
                              let matchDate' = matchDate match 
                              return $ matchDate' > date'

                  when (null results) $ do
                      log $ "Results null for " <> url
                      ret matches

                  when tooOld $ do
                      log $ "Too old for " <> url

                      ret matches
                  return (matches ++ results)

          foldM getResults [] playerURLs
      liftIO $ mapM_ print (take 25 results)
      liftIO $ putStrLn $ "Found " ++ (show $ length results) ++ " Matches for " ++ (T.unpack player)  

log t = liftIO $ putStrLn $ T.unpack $ t


-- |Transform the text argument into a XML-Conduit readable document
getDocument :: T.Text -> WD X.Document
getDocument = return . X.parseText_ X.def . TL.fromStrict

-- |Return HTML source of results page
resultsSource url = do
    openPage url
    waitFor 6500000
    getSource

resultsURL = "http://www.gamedesire.com/#/?dd=16&n=2&sub=1&view=player_results&player=Momsen76&show=archive&gg=103"

matchURLs :: T.Text -> [T.Text]
matchURLs player =  for starts $ \start -> "http://www.gamedesire.com/#/?dd=16&n=2&sub=1&view=player_results&player=" `T.append` player `T.append` "&show=archive&gg=103&start=" `T.append` start

starts = map (T.pack . show) [0, 30 ..]


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
