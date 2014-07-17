{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (join, liftM, void, (>=>))
import           Control.Monad.Error    (ErrorT, runErrorT)
import           Control.Monad.Identity
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans    (lift)
import           Data.ConfigFile
import           Data.Either.Utils
import           Data.String
import           Data.Text              (pack, unpack)
import           Data.Text.Internal     (showText)
import           Data.Time
import           Data.Time.Format
import           Paths
import           System.FilePath
import           System.IO.Unsafe       (unsafePerformIO)
import           System.Locale          (defaultTimeLocale)
import           Test.WebDriver
import           Text.Parsec
import           Text.Parsec.Token
import qualified Text.XML               as X
import qualified Text.XML.Cursor        as C

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

  runSession defaultSession conf $ loginGamedesire user pwd >> resultsSource

parseDoc :: IO X.Document
parseDoc = X.readFile X.def $ fromString resultsFile

parseHoverTable :: X.Document -> [C.Cursor]
parseHoverTable doc = C.fromDocument doc C.$// element "table" >=> (C.attributeIs "class" "hoverTable")

-- parseResultLines :: C.Cursor -> Int
parseResultLines hoverTable = let trs = hoverTable C.$/ element "tbody" C.&/ element "tr"
                                  extractTDs tr = tr C.$/ element "td"
                                  date = parseDate $ extractTDs (trs!!0)!!1
                                  duration = parseDuration $ extractTDs (trs!!0)!!2
                                  players = parsePlayers $ extractTDs (trs!!0)!!3
                                  winner = parseWinner $ extractTDs (trs!!0)!!4
                                  (ranking1, ranking2) = parseRankings $ extractTDs (trs!!0)!!5
                                  (difference1, difference2) = parseDifference $ extractTDs (trs!!0)!!5
                                  bs = parseBS $ extractTDs (trs!!0)!!6
                                  endType = parseEndType $ extractTDs (trs!!0)!!7
                              in  endType

data EndType = TimeOut | Ende | Resign deriving (Show, Read)
parseEndType :: C.Cursor -> EndType
parseEndType td = head . map (read . unwrapText) $ td C.$/ C.content

data BS = JA | NEIN deriving (Show, Read)
parseBS :: C.Cursor -> BS
parseBS td = head . map (read . unwrapText) $ td C.$/ C.content

parseDifference :: C.Cursor -> (Double, Double)
parseDifference td = (r1, r2)
                     where
                     tdContents = map show $ td C.$// element "td" C.&/ C.content
                     r2Str = unwrapString . unwrapString $ tdContents!!2
                     r1Str = unwrapString . unwrapString $ head tdContents
                     r1 = toRankingDouble r1Str
                     r2 = toRankingDouble r2Str
                     toRankingDouble str = (read . map commaToPoint $ drop 1 str) * (signedMultiplier (head str))
                     signedMultiplier plusMinus = if plusMinus == '+' then 1 else -1
                     commaToPoint ',' = '.'
                     commaToPoint c = c

parseRankings :: C.Cursor -> (Double, Double)
parseRankings td =  (ranking1, ranking2)
                   where
                     ranking1 = read . head . map unwrapText $ td C.$// element "font" C.&/ C.content
                     ranking2 = read . unwrapText $ (!!1) $ td C.$// element "td" C.&/ C.content
                     
                                  
parseWinner :: C.Cursor -> [String]
parseWinner td = map unwrapText $ td C.$/ element "b" C.&/ C.content

parsePlayers :: C.Cursor -> (String, String)
parsePlayers td = (player1, player2)
                   where
                    -- playersStr :: C.Cursor  -> [C.Cursor]
                    playersStr td = td C.$// element "tr" C.&/ element "td" C.&/ element "a" C.&/ C.content
                    [player1, player2] = map unwrapText $ playersStr td

type Seconds = Int
parseDuration :: C.Cursor -> Int
parseDuration td = either (const 0) id . parse parseDuration' "parseDuration" . concatMap show $ td C.$/ C.content
                   where
                    parseDuration' :: ParsecT String () Identity Int
                    parseDuration' = do
                      string "\""
                      minutes <- many1 digit
                      char ':'
                      seconds <- many1 digit
                      return $ 60 * read minutes + read seconds

parseDate :: C.Cursor -> UTCTime
parseDate td = readTime defaultTimeLocale formatString dateString
  where
  dateString = concatMap show $ td C.$/ element "span" C.&/ C.content
  formatString = "\"%d.%m.%Y %H:%M:%S\""


hoverTable' = parseDoc >>= return . parseHoverTable

resultLines = hoverTable' >>= return . map parseResultLines

-- unwrapString :: String -> String
unwrapText  = unwrapString . show

unwrapString = reverse . drop 1 . reverse . drop 1



data ResultLine = ResultLine {date        :: UTCTime
                            , duration    :: String
                            , player1     :: String
                            , player2     :: String
                            , winner      :: String
                            , ranking1    :: Float
                            , ranking2    :: Float
                            , difference1 :: Float
                            , difference2 :: Float
                            , ranked      :: Bool
                            , endType     :: String
                            , maxBreak1   :: Int
                            , maxBreak2   :: Int
                           }

resultsSource :: WD ()
resultsSource = do
    liftIO $ threadDelay 3500000

    liftIO $ threadDelay 6500000
    source <- getSource
    liftIO $ writeFile resultsFile $ unpack source
    return ()

xhtml = "{http://www.w3.org/1999/xhtml}"
element = C.element . fromString . (++) xhtml

results :: IO String
results = readFile resultsFile

resultsFile =  "/home/greg/haskell/snooker-statistics/results"



