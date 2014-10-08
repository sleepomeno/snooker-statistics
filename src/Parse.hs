{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}

module Parse (getResultLines)  where

import           Common
import           Control.Monad (join, liftM, void, (>=>))
import           Control.Monad.Identity
import           Data.String
import           Data.Text.Internal
import           Data.Time
import           Data.Time.Format
import           System.Locale (defaultTimeLocale)
import           Text.Parsec
import           Text.Parsec.Token
import qualified Text.XML as X
import qualified Text.XML.Cursor as C

getResultLines :: X.Document -> [ResultLine]
getResultLines = concatMap parseResultLines . parseHoverTable 

parseHoverTable :: X.Document -> [C.Cursor]
parseHoverTable doc = C.fromDocument doc C.$// element "table" >=> (C.attributeIs "class" "hoverTable")

parseResultLines :: C.Cursor -> [ResultLine]
parseResultLines hoverTable = let trs = hoverTable C.$/ element "tbody" C.&/ element "tr"
                                  tdCursors = map (C.$/ element "td") trs :: [[C.Cursor]]
                              in  map parseResultLine tdCursors
                                
parseResultLine tds = ResultLine {..}
  where
    date = parseDate $ tds!!1
    duration = parseDuration $ tds!!2
    (player1, player2) = parsePlayers $ tds!!3
    winner = parseWinner $ tds!!4
    (ranking1, ranking2) = parseRankings $ tds!!5
    (difference1, difference2) = parseDifference $ tds!!5
    ranked = parseBS $ tds!!6
    endType = parseEndType $ tds!!7
    (maxBreak1, maxBreak2) = parseMaxBreaks $ tds!!8

parseMaxBreaks :: C.Cursor -> (Int, Int)
parseMaxBreaks td = (read maxBreak1Str, read maxBreak2Str)
                    where
                      maxBreak1Str = getBreak "font"
                      maxBreak2Str = getBreak "td"
                      getBreak elem = head . map unwrapText $ td C.$// element elem C.&/ C.content

parseEndType :: C.Cursor -> EndType
parseEndType td = head . map (read . unwrapText) $ td C.$/ C.content

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
                     
                                  
parseWinner :: C.Cursor -> String
parseWinner td = head . map unwrapText $ td C.$/ element "b" C.&/ C.content

parsePlayers :: C.Cursor -> (String, String)
parsePlayers td = (player1, player2)
                   where
                    playersStr td = td C.$// element "tr" C.&/ element "td" C.&/ element "a" C.&/ C.content
                    [player1, player2] = map unwrapText $ playersStr td


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

unwrapText :: Text -> String
unwrapText  = unwrapString . show

unwrapString = reverse . drop 1 . reverse . drop 1

xhtml = "{http://www.w3.org/1999/xhtml}"
element = C.element . fromString . (++) xhtml
