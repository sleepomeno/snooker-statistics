{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Main where

import Prelude hiding (writeFile)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Model
import           Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad

import Data.List (groupBy)

import DatabaseUtil
import Database.Persist
import Data.Time.Clock
import Data.Time.Calendar (toGregorian)
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import Control.Monad.Logger (logDebugN, logInfoN, logWarnN, logErrorN)

import Data.Function (on)

import Control.Arrow (first)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Common

-- import Text.Blaze.Html
import Text.Blaze.Html5 as H hiding (map, head)
import Text.Blaze.Html.Renderer.Text  (renderHtml)
import Text.Blaze.Html5.Attributes as A hiding (for)

import Data.Monoid (mconcat)

import Data.Text.IO (writeFile)
import           Paths
import           System.FilePath ((</>))
import           Data.ConfigFile              as C
import           Control.Monad.Error          (ErrorT, runErrorT)
import           Data.Either.Utils

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

main :: IO ()
main = withDB $ do
  matchesFile <- return "/home/greg/haskell/snooker-statistics/hakyll-frontent/lastMatches.markdown"
  matches' <- take 50 <$> selectList [] [Desc MatchDate]
  let matches = map (\(Entity matchId match) -> match) matches' 

  let matchesByDate :: [((Integer, Int, Int), [Match])]
      matchesByDate =  matches' & map (\(Entity matchId match) -> (toGregorian $ utctDay $ matchDate match, match))
                   & groupBy ((==) `on` fst)  & map (first head . unzip)  
  -- logInfoN $ "Return " <> lengthT matches <> " matches"

      header = "---\ntitle: Last matches\n---\n\n"

  let dayHTMLs = mconcat $ for matchesByDate $ \(date, matches) -> do
        H.div ! class_ "matchesOfDay" $ dateHTML date <> (mconcat $ map matchHTML matches)

      output = LT.toStrict $ header <> (renderHtml dayHTMLs)
  
  liftIO $ writeFile matchesFile output
  return ()

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
