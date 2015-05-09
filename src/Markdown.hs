{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards, TupleSections #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Aeson hiding ((.=))
import qualified Data.ByteString.Lazy.Char8    as BL
import           Model
import           Prelude                       hiding (writeFile)

import           Data.List                     (groupBy, nub)

import           Control.Applicative           ((<$>))
import           Control.Monad.Logger          (logDebugN, logErrorN, logInfoN,
                                                logWarnN)
import           Data.Monoid                   ((<>))
import           Data.Time.Calendar            (toGregorian)
import           Data.Time.Clock
import           Database.Persist
import           DatabaseUtil

import           Data.Function                 (on)

import           Control.Applicative
import           Control.Arrow                 (first)

import qualified Control.Foldl                 as L
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as LT

import           Common

-- import Text.Blaze.Html
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              as H hiding (head, map)
import           Text.Blaze.Html5.Attributes   as A hiding (for, id, max)

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


breakOf player match = fromIntegral $ if (matchPlayer1 match == player) then
                                        matchMaxBreak1 match
                                      else
                                        matchMaxBreak2 match

rankingOf player match =  if (matchPlayer1 match == player) then
                            matchRanking1 match
                          else
                            matchRanking2 match

rankingF player = L.Fold (\rankings match -> (rankingOf player match) : rankings) ([] :: [Double]) id

maximumBreak player = L.Fold (\ !break match -> max break (breakOf player match)) 0 id


players = ["herbyger","osterdolfi", "p.hanpe", "_The_Rocket_" , "Momsen76" ]

breakSum player = L.Fold (\ !break match -> break + (breakOf player match)) (0 :: Float) id
durationF = L.Fold (\ duration match -> duration + matchDuration match) (0 :: Int) id

data MatchStats = MatchStats {
    avgBreak      :: Float
  , maxiBreak     :: Int
  , numberMatches :: Int
  , numberWins    :: Int
  , durationAcc   :: Int
  , rankings      :: [Double]
  , stepsResults  :: [Step]
    } deriving (Show, Read)

data Step = Step {
    step    :: Int
  , howMany :: Int
  , won     :: Int
} deriving (Show, Read)

-- plotRanking caption player values = toFile def "example4_big.png" $ do
rankingPlot caption player values = do
    layout_title .= caption
    plot (line player [(zipWith (\i value -> (i, LogValue value)) ([0..] :: [Int]) values)])


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
      statsFold = MatchStats <$> averageBreak' <*> maxBreak' <*> nrMatches' <*> nrWins' <*> durationF <*> rankingF player <*> mconcat (map matchesWithBreak' steps)

      ranges = [10, 20, 30, 50, 100] :: [Int]
      rangesStr :: [String]
      rangesStr = ["ten","twenty","thirty","fifty","hundred","all"]
      breaksOfLast :: Int -> L.Fold Match MatchStats
      breaksOfLast range = case statsFold of
        L.Fold step begin done -> L.Fold (\ (nr, matchStats') match -> if nr < range then (nr+1, step matchStats' match) else (nr, matchStats')) (0,begin) (done . snd)
      allFolds  = mconcat (map (fmap (:[]) . breaksOfLast) ranges ++ [(fmap (:[]) statsFold)])

      -- matchStats :: MatchStats
      -- matchStats = L.fold statsFold matchesOfPlayer

      results = L.fold allFolds matchesOfPlayer :: [MatchStats]

      identifier str = (str <> "-" <> concatMap encode (T.unpack player))
      encode :: Char -> String
      -- encode '_' = "%5F"
      encode '_' = "-"
      encode c = [c]

  let matchStatsToOutput (rangeStr, matchStats) = do

        let inPercent :: Int -> Int -> String
            inPercent n1 0 = "0"
            inPercent n1 n2 = printf "%.2f\n" $ (((100*) $ (fromIntegral $ n1) / (fromIntegral n2)) :: Double)

        let averageBreak :: String
            averageBreak = printf "%.2f\n" $ avgBreak matchStats
            maxBreak :: String
            maxBreak = show . maxiBreak $ matchStats
            nrMatches = numberMatches matchStats
            steps' = stepsResults matchStats
            breaks = for steps' $ \(Step s h w) -> (s, inPercent h nrMatches, h, inPercent w h)
            duration :: String
            duration = let avgDuration :: Int
                           avgDuration = round (((fromIntegral $ durationAcc matchStats) / (fromIntegral nrMatches)) :: Double)
                           (mins, secs) = divMod avgDuration 60
                           show' = printf "%02d"
                           in
                        show' mins <> ":" <> show' secs
            plot = rankingPlot ("Ranking of " <> (T.unpack player)) (T.unpack player) (rankings matchStats)
                        

        let header = "---\ntitle: " <> (LT.pack $ T.unpack player) <> "\n---\n\n"
            stat = H.div ! class_ "stat"
            right = (H.span ! class_ "right") . H.toHtml
            left = H.span ! class_ "left"
            averageOutput = stat $ left "Average Max: " >> right averageBreak
            nrMatchesInAccount = stat $ left "Matches: " >> right (show nrMatches)
            maximumBreak' = stat $ left "Max Break: " >> right maxBreak
            nrWins = stat $ left "Win %: " >> right (inPercent (numberWins matchStats) nrMatches)
            duration' = stat $ left "Average Dauer: " >> right duration

            rankingBox = renderHtml $ H.div ! class_ "rankingBox" $ do
              H.img ! src (H.stringValue $ identifier rangeStr <> ".png") 

            infoBox = renderHtml $ H.div ! class_ "playerInfo" $ do
                nrMatchesInAccount
                nrWins
                duration'
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

            output = LT.toStrict $ header <> infoBox <> breakTable <> rankingBox
        return (rangeStr, output, plot)

  results' <- mapM matchStatsToOutput (zip rangesStr results)

  forM results' $ \(str, output, plot) -> do
    liftIO $ writeFile (breaksDir </> identifier str <> ".markdown") output
    liftIO $ toFile def (breaksDir </> identifier str <> ".png") plot

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
    H.b (H.toHtml $ matchWinner <> " (" <> maxBreakWinner <> ")") <> H.span (H.toHtml $ " schlägt " <> (loser <> " (" <> maxBreakLoser <> ")"))
