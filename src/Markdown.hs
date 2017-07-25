{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, BangPatterns, OverloadedStrings, RecordWildCards, TupleSections #-}

module Main where

import qualified Data.Aeson as A

import qualified Data.Map as M
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import qualified Data.ByteString.Lazy.Char8    as BL
import           Model
import           Folds
import Data.List
import           Prelude                       hiding (writeFile)

import           Data.List                     (groupBy, nub)

import           Control.Applicative           ((<$>))
import           Control.Monad.Logger          (logDebugN, logErrorN, logInfoN,
                                                logWarnN)
import           Data.Monoid                   ((<>), mempty)
import           Data.Time.Calendar            (toGregorian)
import           Data.Time.Clock
import           Database.Persist
import           DatabaseUtil

import           Data.Function                 (on)
import           Data.Maybe


import           Control.Applicative
import           Control.Arrow                 (first)

import qualified Control.Foldl                 as L
import qualified Data.Text                     as T
import qualified Data.Text.Encoding                     as TE
import qualified Data.Text.Lazy                as LT

import           Common

-- import Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              as H hiding (head, map, main)
import           Text.Blaze.Html5.Attributes   as A hiding (for, max, min)

import           Data.Monoid                   (mconcat)

import           Control.Monad.Error           (ErrorT, runErrorT)
import           Data.ConfigFile               as C
import           Data.Either.Utils
import qualified Data.Set                      as S
import           Data.Text.IO                  (writeFile)
import           Paths
import           System.FilePath               ((</>))
import           Text.Printf

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Data.Time
import Data.Time.Format


rankingPlot caption player values = do
    layout_title .= caption
    plot (line player [(zipWith (\i value -> (i, LogValue value)) ([0..] :: [Int]) values)])

encode :: Char -> String
encode '_' = "-"
encode c = [c]

stat = H.div ! class_ "stat"
right = (H.span ! class_ "right") . H.toHtml
left = H.span ! class_ "left"


getMatchesOfPlayer player = do
  allMatchesOfPlayer <- lift $ selectList ([MatchPlayer1 ==. player] ||. [MatchPlayer2 ==. player]) [Desc MatchDate]

  let rankingNotZero match = rankingOf player match /= 0

      filterMatches :: [Entity Match] -> [Match]
      filterMatches = filter rankingNotZero . S.toList . S.fromList . map (\(Entity _ match) -> match)

  return $ filterMatches allMatchesOfPlayer 
  
matchStatsToOutput player matchesOfPlayer (rangeStr, (range, matchStats)) = do

    let inPercent :: Int -> Int -> String
        inPercent n1 0 = "0"
        inPercent n1 n2 = printf "%.2f\n" $ (((100*) $ (fromIntegral $ n1) / (fromIntegral n2)) :: Double)
        playerEncoded :: String
        playerEncoded = concatMap encode . T.unpack $ player
        identifier str = str <> "-" <> playerEncoded

    let averageBreak :: String
        averageBreak = printf "%.2f\n" $ avgBreak matchStats
        maxBreak :: String
        maxBreak = show . maxiBreak $ matchStats
        nrMatches = numberMatches matchStats
        bSeries = case bestSeries matchStats of { BestSeries x -> show x }
        wSeries = case worstSeries matchStats of { WorstSeries x -> show x }
        rankingDiff' = printf "%.2f\n" $ rankingDiff matchStats
        lastRanking' = printf "%.2f\n" $ lastRanking matchStats
        steps' = stepsResults matchStats
        breaks = for steps' $ \(Step s h w) -> (s, inPercent h nrMatches, h, inPercent w h)
        duration :: String
        duration = let avgDuration :: Int
                       avgDuration = round (((fromIntegral $ durationAcc matchStats) / (fromIntegral nrMatches)) :: Double)
                       (mins, secs) = divMod avgDuration 60
                       show' = printf "%02d"
                        in
                    show' mins <> ":" <> show' secs
        plot = rankingPlot ("Ranking of last " <> show nrMatches <> " matches") playerEncoded (rankings matchStats)


    let metadata = "---\ntitle: " <> (LT.pack $ playerEncoded) <> "\n---\n\n"
        averageOutput = stat $ left "Average Max: " >> right averageBreak
        nrMatchesInAccount = stat $ left "Matches: " >> right (show nrMatches)
        maximumBreak' = stat $ left "Max Break: " >> right maxBreak
        winPerc = inPercent (numberWins matchStats) nrMatches
        nrWins = stat $ left "Win %: " >> right winPerc
        duration' = stat $ left "Average Duration: " >> right duration
        rankingDiff'' = stat $ left "Ranking Difference: " >> right rankingDiff'
        lastRanking'' = stat $ left "Ranking: " >> right lastRanking'
        bSeries' = stat $ left "Best Run: " >> right bSeries
        wSeries' = stat $ left "Worst Run: " >> right wSeries

        rankingBox = renderHtml $ H.div ! class_ "rankingBox" $ do
            H.img ! src (H.stringValue $ identifier rangeStr <> ".png")

        infoBox = renderHtml $ H.div ! class_ "playerInfo" $ do
            nrMatchesInAccount
            nrWins
            duration'
            maximumBreak'
            averageOutput
            lastRanking''
            rankingDiff''
            wSeries'
            bSeries'
    let maxMatchesShown = maxBound
        nrShownMatches = min range maxMatchesShown
        matchesOfPlayerInRange = take range matchesOfPlayer
    output <- matchesMarkdown matchesOfPlayerInRange
    let matchesBox = renderHtml $ H.div ! class_ "matchesBox" $ output
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

        output = LT.toStrict $ metadata <> infoBox <> breakTable <> rankingBox <> matchesBox

    let breakStats' = for breaks $ \(m,p,a,w) -> BreakStat (show m) p (show a) w
        playerBreakStat = PlayerBreakStat rangeStr (min range $ length matchesOfPlayerInRange)  winPerc duration maxBreak averageBreak lastRanking' rankingDiff'  breakStats' bSeries wSeries
    -- lift $ BL.writeFile (breaksDir </> identifier rangeStr <> ".json") playerBreakStat


    return (rangeStr, output, plot, playerBreakStat)

breaksMarkdown player = do
  matchesOfPlayer <- getMatchesOfPlayer player

  let 
      rangesStr :: [String]
      rangesStr = ["Last 10 Matches","Last 50 Matches","Last 100 Matches","All Matches", "Last Session", "Last 3 days"]

  
      results :: [(Int, MatchStats)]
      results = L.fold (allFolds player) matchesOfPlayer 
      playerEncoded :: String
      playerEncoded = concatMap encode . T.unpack $ player
      identifier str = str <> "-" <> playerEncoded



  results' <- mapM (matchStatsToOutput player matchesOfPlayer) (zip rangesStr results)

  let statsJsons = map (\(str, _, _, pBS) -> pBS) results'
      statJson = A.encode $ statsJsons
      matchesJson = A.encode $ matchesOfPlayer


      metadata = T.pack $ "---\ntitle: " <> playerEncoded <> "\n---\n\n"
      wrapperBox = LT.toStrict $ renderHtml $ H.div ! A.id "wrapper" $ do
        H.script $ H.text $ "var statistics = " <> (TE.decodeUtf8 . BL.toStrict $ statJson) <> ";\n"
                         <> "var matchesOfPlayer = " <> (TE.decodeUtf8 . BL.toStrict $ matchesJson) <> ";\n"
                         <> "var playerName = '" <> T.pack playerEncoded  <> "';"
        H.script ! src "../js/player.js" $ mempty


  breaksDir <- T.unpack <$> asks outputDir
  liftIO $ writeFile (breaksDir </> ("player-" <> playerEncoded <> ".html")) (metadata <> wrapperBox)

  forM results' $ \(str, output, plot, _) -> do
    liftIO $ writeFile (breaksDir </> identifier str <> ".html") output
    liftIO $ toFile def (breaksDir </> identifier str <> ".png") plot

matchesMarkdown matches = do
  let matchesByDate :: [((Integer, Int, Int), [Match])]
      matchesByDate =  matches & map (\match -> (toGregorian $ utctDay $ matchDate match, match))
                   & groupBy ((==) `on` fst)  & map (first head . unzip)


  let dayHTMLs = mconcat $ for matchesByDate $ \(date, matches) -> do
        H.div ! class_ "matchesOfDay" $ dateHTML date <> (H.table ! class_ "matchesTable" $ (mconcat $ map matchHTML matches))

  return dayHTMLs

lastMatchesMarkdown = do
  matches' <- lift $ selectList [] [Desc MatchDate]
  let matches = take 100 . S.toList . S.fromList $ map (\(Entity matchId match) -> match) matches'
      header = "---\ntitle: Last 100 matches\n---\n\n"
  output <- matchesMarkdown matches
  let result = LT.toStrict $ header <> renderHtml output
  file <- asks lastMatchesFile
  liftIO $ writeFile (T.unpack file) result

dateHTML (year, month, day) = do
  let year' = H.toHtml year
  let month' = H.toHtml month
  let day' = H.toHtml day

  H.div $ H.h3 $
    day' <> "." <> month' <> "." <> year'

matchHTML (Match{..}) = do
  let show' :: (Num a, Show a) => a -> T.Text
      show' = T.pack . show
      show'' = T.pack . show . round
      markupPlayer player = if matchWinner == player then
                              H.b . H.text $ player
                            else
                              H.text $ player
      ply1Td = H.td ! class_ "player1"
      ply2Td = H.td ! class_ "player2"
  H.tr $
    ((H.td ! class_ "matchDate") . H.text $ T.pack $ formatTime defaultTimeLocale "%H:%M" matchDate)  <>
    (ply1Td . markupPlayer $ matchPlayer1 ) <>
    (ply1Td . H.text $ show'' matchRanking1 ) <>
    (ply1Td . H.i . H.text $ show' matchMaxBreak1 ) <>
    (ply2Td . markupPlayer $ matchPlayer2) <>
    (ply2Td . H.text $ show'' matchRanking2) <>
    (ply2Td . H.i . H.text $ show' matchMaxBreak2 )

rivalries' = do
  users <- asks players
  oldUsers <- asks oldPlayers
  rivalUsers' <- asks rivalUsers
  let allUsers = users ++ rivalUsers' ++ oldUsers
  let duels = nub $ map sort [[pl1, pl2] | pl1 <- allUsers, pl2 <- allUsers, pl1 /= pl2]
  mapM rivalry duels

data Rivalry = Rivalry {
    nrMatches :: Int
  , numberWinsPl1    :: Int
  , numberWinsPl2    :: Int
  , diffsPl1   :: [Double]
  , diffsPl2   :: [Double]
  , diffPl1   :: Double
  , diffPl2   :: Double
    } deriving (Show, Read)

round' f = ((/100) $ fromIntegral $ round (f * 100))
diffsF player = L.Fold (\(diffs, current) match -> let new = current + (rankingDiffOf player match) in (new : diffs, new)) ([], 0) (fst)
diffF player = L.Fold (\ diff match -> diff + (rankingDiffOf player match)) (0 :: Double) Prelude.id
winsF player = L.Fold (\ nr match -> if matchWinner match == player then nr + 1 else nr) 0 Prelude.id

encodeName player = concatMap encode (T.unpack player)

rivalry [pl1, pl2] = do
  rivalries'' <- T.unpack <$> asks rivalries
  matches <- S.toList . S.fromList . map (\(Entity _ match) -> match) <$>  (lift $ selectList ([MatchPlayer1 ==. pl1, MatchPlayer2 ==. pl2] ||. [MatchPlayer1 ==. pl2, MatchPlayer2 ==. pl1]) [Asc MatchDate])
  let rivalF = Rivalry <$> L.genericLength <*> winsF pl1 <*> winsF pl2 <*> diffsF pl1 <*> diffsF pl2 <*> diffF pl1 <*> diffF pl2
      Rivalry{..} = L.fold rivalF $ matches
      plot = duelPlot pl1 diffsPl1 pl2 diffsPl2

      filename = encodeName pl1 <> "-vs-" <> encodeName pl2

  let metadata = "---\ntitle: " <> (LT.pack $ T.unpack pl1) <> " vs " <> (LT.pack $ T.unpack pl2) <> "\n---\n\n"
      plBox (player,numberWins,diff) = H.div ! class_ "playerBox" $ do
        H.h3 . H.text $ player
        H.div ! class_ "playerWins" $ stat $ left "Wins: " >> right (show numberWins)
        H.div ! class_ "playerDiff" $ stat $ left "Ranking: " >> right (printf "%.2f\n" diff)
      playerBoxes = renderHtml $ mapM_ plBox [(pl1, numberWinsPl1, diffPl1), (pl2, numberWinsPl2, diffPl2)]

      rankingBox = renderHtml $ H.div ! class_ "rankingBox" $ do
        H.img ! src (H.stringValue $ filename <> ".png")

  matchesOutput <- matchesMarkdown (reverse matches)

  let matchesBox = renderHtml $ H.div ! class_ "matchesBox" $ matchesOutput
      output = LT.toStrict $ metadata <> playerBoxes <> rankingBox <> matchesBox

  unless (length matches == 0) $ do
    liftIO $ toFile def (rivalries'' </> filename <> ".png") plot
    liftIO $ writeFile (rivalries'' </> filename <> ".html") output

duelPlot player1 values1 player2 values2 = do
    layout_title .= "Ranking Differences"
    plot (line (T.unpack player1) [dataOf values1])
    plot (line (T.unpack player2) [dataOf values2])
    where
      dataOf values = zipWith (\i value -> (i, value)) ([0..] :: [Int]) $ reverse values

main :: IO ()
main = withDB $ void $ do
  conf <- liftIO readConfig
  flip runReaderT conf $ do
    lastMatchesMarkdown
    mapM_ breaksMarkdown (players conf ++ oldPlayers conf)
    rivalries'

doRivalries :: IO ()
doRivalries = withDB . void $ do
  conf <- liftIO readConfig
  flip runReaderT conf rivalries'
