{-# LANGUAGE ScopedTypeVariables, FlexibleContexts, TypeFamilies, BangPatterns, OverloadedStrings, RecordWildCards, TupleSections #-}

module Folds
       (
         allFolds
       ) where
import           Model
import qualified Control.Foldl                 as L
import qualified Data.Text                     as T
import Data.Time
import Data.Time.Format
import           Data.Monoid                   (mconcat)
import           Control.Monad
import           Common
import           Data.Maybe
import Data.List


breakOf :: Num b => T.Text -> Match -> b
breakOf player match = fromIntegral $ if (matchPlayer1 match == player) then
                                        matchMaxBreak1 match
                                      else
                                        matchMaxBreak2 match

rankingF :: T.Text -> L.Fold Match [Double]
rankingF player = L.Fold (\rankings match -> (rankingOf player match) : rankings) ([] :: [Double]) Prelude.id

maximumBreak :: (Ord b, Num b) => T.Text -> L.Fold Match b
maximumBreak player = L.Fold (\ !break match -> max break (breakOf player match)) 0 Prelude.id


breakSum :: T.Text -> L.Fold Match Float
breakSum player = L.Fold (\ !break match -> break + (breakOf player match)) (0 :: Float) Prelude.id

durationF :: L.Fold Match Int
durationF = L.Fold (\ duration match -> duration + matchDuration match) (0 :: Int) Prelude.id

rankingDiffF :: T.Text -> L.Fold Match Double
rankingDiffF player = L.Fold (\ diff match -> diff + rankingDiffOf player match) (0 :: Double) Prelude.id

lastRankingF :: T.Text -> L.Fold Match Double
lastRankingF player = L.Fold (\ ranking match -> Just $ case ranking of
                                 Nothing -> rankingOf player match
                                 Just x  -> x) (Nothing :: Maybe Double) fromJust

nrWins' :: Num b => T.Text -> L.Fold Match b
nrWins' player = L.Fold (\ !nr match -> if matchWinner match == player then nr +1 else nr) 0 Prelude.id

bestSeries' :: T.Text -> L.Fold Match BestSeries
bestSeries' player = L.Fold (\( ! best, ! current) match -> if matchWinner match == player then
                                                    let current' = current + 1 in
                                                        if current' > best then (current', current') else (best, current')
                                                    else
                                                    (best, 0)) (0,0) (\(best, _) -> BestSeries best)
worstSeries' :: T.Text -> L.Fold Match WorstSeries
worstSeries' player = L.Fold (\( ! worst, ! current) match -> if matchWinner match /= player then
                                                    let current' = current + 1 in
                                                        if current' > worst then (current', current') else (worst, current')
                                                    else
                                                    (worst, 0)) (0,0) (\(worst, _) -> WorstSeries worst)
matchesWithBreak' :: T.Text -> Int -> L.Fold Match [Step]
matchesWithBreak' player break = L.Fold (\(! nr, !won) match -> if (breakOf player match >= break) then
                                                    (nr+1, if matchWinner match == player then won + 1 else won)
                                                    else
                                                    (nr, won)) (0,0) (\(nr, won) -> [Step break nr won])
averageBreak :: T.Text -> L.Fold Match Float
averageBreak player = (/) <$> breakSum player <*> L.genericLength

statsFold :: T.Text -> L.Fold Match MatchStats
statsFold player = MatchStats <$> averageBreak player <*> maximumBreak player <*> L.genericLength <*> nrWins' player <*> durationF <*> rankingF player <*> rankingDiffF player <*> lastRankingF player <*> mconcat (map (matchesWithBreak' player) steps) <*> bestSeries' player <*> worstSeries' player
                   where
                     steps = [10, 20 .. 140] ++ [147]

breaksOfLast :: L.Fold Match MatchStats -> Int -> L.Fold Match (Int, MatchStats)
breaksOfLast statsFold range = case statsFold of
  L.Fold step begin done -> L.Fold (\ (nr, matchStats') match -> if nr < range then
                                                                    (nr+1, step matchStats' match)
                                                                else
                                                                    (nr, matchStats'))
                            (0,begin) (\(_, m) -> (range, done m))

lastSession :: L.Fold Match MatchStats -> L.Fold Match (Int, MatchStats)
lastSession statsFold = case statsFold of
  L.Fold step begin done -> L.Fold (\ (time, !nr, matchStats') match ->
                                    let time' = matchDate match
                                        in
                                    (Just time', nr+1, step matchStats' match) `fromMaybe` (do
                                        timeJ <- time
                                        guard $ timeJ `diffUTCTime` time' >= sessionLimit
                                        return (time, nr, matchStats'))) (Nothing, 0, begin) (\(t, n, m) -> (n, done m))
  where
      sessionLimit = 4200

lastDays :: L.Fold Match MatchStats -> NominalDiffTime -> L.Fold Match (Int, MatchStats)
lastDays statsFold days = case statsFold of
  L.Fold step begin done -> L.Fold (\ (time, !nr, matchStats') match ->
                                    let time' = matchDate match
                                        next = time `mplus` (Just time')
                                        in
                                    (next, nr+1, step matchStats' match) `fromMaybe` (do
                                        timeJ <- next
                                        guard $ (timeJ `diffUTCTime` time') >= (days * 60*60*24)
                                        return (next, nr, matchStats'))) (Nothing, 0, begin) (\(t, n, m) -> (n, done m))


allFolds :: T.Text -> L.Fold Match [(Int, MatchStats)]
allFolds player = let matchFold = statsFold player
                      ranges = [10, 50, 100, maxBound] :: [Int]
                      in
                   mconcat $ (map (fmap (:[]) . breaksOfLast matchFold) ranges) ++ [fmap (:[]) (lastSession matchFold)] ++ [fmap (:[]) $ lastDays matchFold 3]
