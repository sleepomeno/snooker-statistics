{-# LANGUAGE BangPatterns, TupleSections, RecordWildCards, OverloadedStrings #-}

module Main where

import Prelude hiding (writeFile)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Model
import           Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad

import Data.List (groupBy, nub)

import DatabaseUtil
import Database.Persist
import Data.Time.Clock
import Data.Time.Calendar (toGregorian)
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import Control.Monad.Logger (logDebugN, logInfoN, logWarnN, logErrorN)

import Data.Function (on)

import Control.Arrow (first)
import Control.Applicative

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Control.Foldl as L

import Common

-- import Text.Blaze.Html
import Text.Blaze.Html5 as H hiding (map, head)
import Text.Blaze.Html.Renderer.Text  (renderHtml)
import Text.Blaze.Html5.Attributes as A hiding (for, id, max)

import Data.Monoid (mconcat)

import Data.Text.IO (writeFile)
import           Paths
import           System.FilePath ((</>))
import           Data.ConfigFile              as C
import           Control.Monad.Error          (ErrorT, runErrorT)
import           Data.Either.Utils
import Text.Printf
import qualified Data.Set as S


breakOf player match = fromIntegral $ if (matchPlayer1 match == player) then
                                        matchMaxBreak1 match
                                      else
                                        matchMaxBreak2 match

maximumBreak player = L.Fold (\ !break match -> max break (breakOf player match)) 0 id


players = ["osterdolfi", "p.hanpe", "_The_Rocket_" , "Momsen76" ]

breakSum player = L.Fold (\ !break match -> break + (breakOf player match)) (0 :: Float) id

data MatchStats = MatchStats {
    avgBreak :: Float
  , maxiBreak     :: Int
  , numberMatches    :: Int
  , numberWins :: Int
  , stepsResults        :: [Step]
    } deriving (Show, Read)

data Step = Step {
    step     :: Int
  , howMany  :: Int
  , won      :: Int
} deriving (Show, Read)


breaksMarkdown player = do
  let breaksDir = "/home/greg/haskell/snooker-statistics/hakyll-frontent/breaks" :: FilePath
  let matchesWith opts = S.toList . S.fromList . map (\(Entity _ match) -> match) <$>  selectList opts [Desc MatchDate]

  let matchesWithBreak break = matchesWith $ [MatchPlayer1 ==. player, MatchMaxBreak1 >=. break] ||. [MatchPlayer2 ==. player, MatchMaxBreak2 >=. break]

  matchesOfPlayer <- matchesWith ([MatchPlayer1 ==. player] ||. [MatchPlayer2 ==. player])
  let steps = [10, 20 .. 140] ++ [147]

  let averageBreak' = (/) <$> breakSum player <*> L.genericLength
      maxBreak' = maximumBreak player
      nrMatches' = L.genericLength

      nrWins' = L.Fold (\ !nr match -> if matchWinner match == player then nr +1 else nr) 0 id

      matchesWithBreak' break = L.Fold (\(! nr, !won) match -> if (breakOf player match >= break) then
                                                            (nr+1, if matchWinner match == player then won + 1 else won)
                                                           else
                                                            (nr, won)) (0,0) (\(nr, won) -> [Step break nr won])
      matchStats :: MatchStats
      matchStats = L.fold (MatchStats <$> averageBreak' <*> maxBreak' <*> nrMatches' <*> nrWins' <*> mconcat (map matchesWithBreak' steps)) matchesOfPlayer

  let inPercent :: Int -> Int -> String
      inPercent n1 0 = "0"
      inPercent n1 n2 = printf "%.2f\n" $ (((100*) $ (fromIntegral $ n1) / (fromIntegral n2)) :: Double)
  
  let averageBreak :: String
      averageBreak = printf "%.2f\n" $ avgBreak matchStats
      maxBreak :: String
      maxBreak = show $ maxiBreak matchStats
      nrMatches = numberMatches matchStats
      steps' = stepsResults matchStats
      breaks = for steps' $ \(Step s h w) -> (s, inPercent h nrMatches, h, inPercent w h)

  let header = "---\ntitle: " <> (LT.pack $ T.unpack player) <> "\n---\n\n"
      -- asH2 = H.h2 . H.toHtml
      asH2 = H.div ! class_ "stat"
      right = (H.span ! class_ "right") . H.toHtml
      left = H.span ! class_ "left"
      averageOutput = asH2 $ left "Average Max: " >> right averageBreak
      nrMatchesInAccount = asH2 $ left "Matches: " >> right (show nrMatches)
      maximumBreak' = asH2 $ left "Max Break: " >> right maxBreak
      nrWins = asH2 $ left "Win %: " >> right (inPercent (numberWins matchStats) nrMatches)
      infoBox = renderHtml $ H.div ! class_ "playerInfo" $ do
        nrMatchesInAccount
        nrWins
        maximumBreak'
        averageOutput

  let allBreaks = mconcat $ for breaks $ \(step, percent, absolute, won) -> do
        H.tr $ do
           H.td $ H.span (H.toHtml step)
           H.td $ H.span (H.toHtml percent)
           H.td $ H.span (H.toHtml absolute)
           H.td $ H.span (H.toHtml won)
      breakTable = renderHtml $ H.table ! class_ "breaksTable" $ do
        H.thead $ H.tr $ do 
            H.th (H.span "Break >=")
            H.th (H.span "Prozent")
            H.th (H.span "Absolut")
            H.th (H.span "Win %")
        H.tbody $ allBreaks
      
      output = LT.toStrict $ header <> infoBox <> breakTable
      encode :: Char -> String
      encode '_' = "%5F"
      encode c = [c]
  
  liftIO $ writeFile (breaksDir </> (concatMap encode (T.unpack player) <> ".markdown")) output

lastMatchesFile :: IO String
lastMatchesFile = do
  dataDir <- getStaticDir
  let configFile = dataDir </> "config.txt"
      readProp p  = C.get p "DEFAULT"

  eitherConfig <- runErrorT $ do
    parser <- join $ liftIO $ readfile emptyCP configFile
    file <- readProp parser "lastmatchesfile"
    return file

  let file = forceEither eitherConfig
  return file

lastMatchesMarkdown = do 
  matchesFile <- return "/home/greg/haskell/snooker-statistics/hakyll-frontent/lastMatches.markdown"
  matches' <- selectList [] [Desc MatchDate]
  let matches = take 50 . S.toList . S.fromList $ map (\(Entity matchId match) -> match) matches' 

  let matchesByDate :: [((Integer, Int, Int), [Match])]
      matchesByDate =  matches & map (\match -> (toGregorian $ utctDay $ matchDate match, match))
                   & groupBy ((==) `on` fst)  & map (first head . unzip)  

      header = "---\ntitle: Last matches\n---\n\n"

  let dayHTMLs = mconcat $ for matchesByDate $ \(date, matches) -> do
        H.div ! class_ "matchesOfDay" $ dateHTML date <> (mconcat $ map matchHTML matches)

      output = LT.toStrict $ header <> (renderHtml dayHTMLs)
  
  liftIO $ writeFile matchesFile output

main :: IO ()
main = withDB $ do
  lastMatchesMarkdown
  mapM_ breaksMarkdown players

dateHTML (year, month, day) = do
  let year' = H.toHtml year
  let month' = H.toHtml month
  let day' = H.toHtml day

  H.div $ H.h2 $
    day' <> "." <> month' <> "." <> year'
    
matchHTML (Match{..}) = do
  let loser = if matchWinner == matchPlayer1 then matchPlayer2 else matchPlayer1
      maxBreakWinner = T.pack . show $ if matchWinner == matchPlayer1 then matchMaxBreak1 else matchMaxBreak2
      maxBreakLoser = T.pack . show $ if loser == matchPlayer1 then matchMaxBreak1 else matchMaxBreak2
  H.div $
    H.b (H.toHtml $ matchWinner <> " (" <> maxBreakWinner <> ")") <> H.span (H.toHtml $ " beats " <> (loser <> " (" <> maxBreakLoser <> ")"))
