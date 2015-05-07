{-# LANGUAGE TupleSections, RecordWildCards, OverloadedStrings #-}

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

maximumBreak player = L.Fold (\break match -> max break (breakOf player match)) 0 id



players = ["osterdolfi", "p.hanpe", "_The_Rocket_" , "Momsen76" ]
-- players = ["osterdolfi", "p.hanpe", "Momsen76"]

-- breakSum :: T.Text -> L.Fold (Entity (MatchGeneric backend)) Float
breakSum player = L.Fold (\break match -> break + (breakOf player match)) (0 :: Float) id

breaksMarkdown player = do
  let breaksDir = "/home/greg/haskell/snooker-statistics/hakyll-frontent/breaks" :: FilePath
  let matchesWith opts =  selectList opts []
  let matchesWithBreak break = matchesWith $ [MatchPlayer1 ==. player, MatchMaxBreak1 >=. break] ||. [MatchPlayer2 ==. player, MatchMaxBreak2 >=. break]
  matchesOfPlayer <- S.toList . S.fromList . map (\(Entity _ match) -> match) <$> (matchesWith ([MatchPlayer1 ==. player] ||. [MatchPlayer2 ==. player]))

  -- let average = L.fold (breakSum player) matchesOfPlayer
  let averageBreak :: String
      averageBreak = printf "%.2f\n" $ L.fold ((/) <$> (breakSum player) <*> L.genericLength) matchesOfPlayer
      maxBreak :: String
      maxBreak = show $ L.fold (maximumBreak player) matchesOfPlayer
  
  let nrMatches = length matchesOfPlayer
  liftIO $ putStrLn $ show nrMatches
  let steps = [20, 30 .. 140] ++ [147]

  let ofAll :: [a] -> String
      ofAll matches = printf "%.2f\n" $ (((100*) $ (fromIntegral $ length matches) / (fromIntegral nrMatches)) :: Float)

  breaks <- forM steps $ \step -> do
    (step,) <$> (fmap ofAll $ matchesWithBreak step)

  let header = "---\ntitle: " <> (LT.pack $ T.unpack player) <> "\n---\n\n"
      asH2 = renderHtml . H.h2 . H.toHtml
      averageOutput = asH2 $ "Average Break: " <> averageBreak
      nrMatchesInAccount = asH2 $ (show nrMatches) <> " Matches in Statistik"
      maximumBreak' = asH2 $ "Max Break: " <> maxBreak

  let allBreaks = mconcat $ for breaks $ \(step, part) -> do
        H.tr $ do
           H.td $ H.span (H.toHtml step)
           H.td $ H.span (H.toHtml part )
      breakTable = renderHtml $ H.table $ do
        H.thead $ H.tr $ do 
            H.td (H.span "Mehr als")
            H.td (H.span "In Prozent")
        H.tbody $ allBreaks
      
      output = LT.toStrict $ header <> nrMatchesInAccount <> maximumBreak' <> averageOutput <> breakTable
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
  matches' <- take 50 <$> selectList [] [Desc MatchDate]
  let matches = S.toList . S.fromList $ map (\(Entity matchId match) -> match) matches' 

  let matchesByDate :: [((Integer, Int, Int), [Match])]
      matchesByDate =  matches' & map (\(Entity matchId match) -> (toGregorian $ utctDay $ matchDate match, match))
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
