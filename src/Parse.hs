{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards          #-}

module Parse where

import           Common
import           Control.Monad (join, liftM, void, (>=>))
import           Control.Monad.Identity
import           Data.String
import           Data.Either.Combinators
import Control.Applicative ((<*),(*>))
import           Data.Text.Internal
import           Data.Time
import           Data.Time.Format
import           System.Locale (defaultTimeLocale)
import           Text.Parsec
import           Text.Parsec.Token
import qualified Data.Text as T
import qualified Text.XML as X
import qualified Text.XML.Cursor as C
import  Text.XML.Cursor  hiding (element)

import Model

import Debug.Trace


-- |Returns all the games displayed 
getResultLines :: X.Document -> [Match]
getResultLines = concatMap parseResultLines . parseHoverTable . fromDocument

-- |Lookups all tables with class 'hoverTable'
parseHoverTable :: Cursor -> [Cursor]
parseHoverTable doc =  doc $// element "table" >=> classIs "hoverTable"

-- |Applies parseResultLine to all rows of the hoverTable
parseResultLines :: Cursor -> [Match]
parseResultLines hoverTable = let trs = hoverTable $/ element "tbody" &/ element "tr"
                                  -- tdCursors = map ($/ element "td") trs :: [[Cursor]]
                                  tdCursors = hoverTable $/ element "tbody" &/ element "tr" &| child >=> element "td"
                              in  map parseResultLine tdCursors

                                
-- |Calls the parsing functions for the specific column and puts the results together
parseResultLine tds = Match {..}
  where
    matchDate = parseDate $ tds!!1
    matchDuration = parseDuration $ tds!!2
    (matchPlayer1, matchPlayer2) = parsePlayers $ tds!!3
    matchWinner = parseWinner $ tds!!4
    (matchRanking1, matchRanking2) = parseRankings $ tds!!5
    (matchDifference1, matchDifference2) = parseDifference $ tds!!5
    matchRanked = parseBS $ tds!!6
    matchEndType = parseEndType $ tds!!7
    (matchMaxBreak1, matchMaxBreak2) = parseMaxBreaks $ tds!!8


----------------------------------------------
--------- ResultLine parsing methods ---------
----------------------------------------------
parseMaxBreaks :: Cursor -> (Int, Int)
parseMaxBreaks td = (read maxBreak1Str, read maxBreak2Str)
                    where
                      maxBreak1Str = getBreak "font"
                      maxBreak2Str = getBreak "td"
                      getBreak elem = head . map unwrapText $ td $// element elem &/ content

parseEndType :: Cursor -> EndType
parseEndType td = head . map (read . unwrapText) $ td $/ content

parseBS :: Cursor -> BS
parseBS td = head . map (read . unwrapText) $ td $/ content

parseDifference :: Cursor -> (Double, Double)
parseDifference td = (r1, r2)
                     where
                     tdContents = map show $ td $// element "td" &/ content
                     r2Str = tdContents!!2
                     r1Str = head tdContents
                     r1 = toRankingDouble r1Str
                     r2 = toRankingDouble r2Str
                     toRankingDouble str = let result = parse parseRankingDouble "parseRankingDouble" str
                                               in
                                            (either (const (error "could not parse difference")) id result) -- TODO need error handling
                     parseRankingDouble :: ParsecT String () Identity Double
                     parseRankingDouble = do
                       anyChar >> anyChar
                       multiplier <- option 1 $ (char '+' *> return 1) <|> (char '-' *> return (-1))
                       n1 <- many digit
                       n2 <- option "0" $ do
                         char ','
                         many digit
                       anyChar >> anyChar
                       return $ multiplier * (read (n1 ++ "." ++ n2) :: Double)
                       

parseRankings :: Cursor -> (Double, Double)
parseRankings td =  (ranking1, ranking2)
                   where
                     ranking1 = read . head . map unwrapText $ td $// element "font" &/ content
                     ranking2 = read . unwrapText $ (!!1) $ td $// element "td" &/ content
                     
                                  
parseWinner :: Cursor -> T.Text
parseWinner td = head $ td $/ element "b" &/ content

parsePlayers :: Cursor -> (T.Text, T.Text)
parsePlayers td = (player1, player2)
                   where
                    playersStr td = td $// element "tr" &/ element "td" &/ anyElement &/ content
                    [player1, player2] = playersStr td


parseDuration :: Cursor -> Int
parseDuration td = either (const 0) id . parse parseDuration' "parseDuration" . concatMap show $ td $/ content
                   where
                    parseDuration' :: ParsecT String () Identity Int
                    parseDuration' = do
                      string "\""
                      minutes <- many1 digit
                      char ':'
                      seconds <- many1 digit
                      return $ 60 * read minutes + read seconds

parseDate :: Cursor -> UTCTime
parseDate td = readTime defaultTimeLocale formatString dateString
  where
  dateString = concatMap show $ td $/ element "span" &/ content
  formatString = "\"%d.%m.%Y %H:%M:%S\""

-- remove the first and last character
unwrapText :: Text -> String
-- unwrapText  = read . show . T.reverse . T.drop 1 . T.reverse . T.drop 1
unwrapText  = unwrapString . show

unwrapString = reverse . drop 1 . reverse . drop 1


------- Whenever you lookup an element you must include the xhtml namespace
xhtml = "{http://www.w3.org/1999/xhtml}"
element = C.element . fromString . (++) xhtml

classIs = attributeIs "class" 


