module Main where

import Data.Void
import Data.Tuple
import Data.Either
import Data.Maybe.Unsafe
import Data.Maybe
import Data.Function (on)

import Data.Profunctor.Strong (first)
import Control.Bind
import Control.Monad.Eff

import DOM
import Data.Date
import Data.Date.UTC
import Data.Time
import Data.Int

import qualified Data.Map as M       
import Data.Foldable       
import Data.Traversable

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Data.Array (take, groupBy, head)
import qualified Data.Array.Unsafe as AU
import Halogen
import Halogen.Signal hiding (head)
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A

-- for = flip (<$>)

appendToWrapper :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToWrapper e = do
  doc <- document globalWindow
  (Just content) <- querySelector "#wrapper" doc
  appendChild content e

type Statistics = { matches       :: Number
                  , label         :: String
                  , winPercentage :: String
                  , avgDuration   :: String
                  , maxBreak      :: String
                  , avgMax        :: String
                  , ranking       :: String
                  , rankDiff      :: String
                  , breakStats    :: [Break]
                  }

type Break = { minimum       :: String
             , percent       :: String
             , absolute      :: String
             , wPercentage :: String
             }

type Day = { day            :: String
           , matches        :: [Match]
           }

type Match = {
               date           :: String
             , duration       :: String
             , winner         :: String
             , player1        :: String
             , ranking1       :: String
             , maxBreak1         :: String
             , player2        :: String
             , ranking2       :: String
             , maxBreak2         :: String
             }

foreign import statistics :: [Statistics]
foreign import matchesOfPlayer :: [Match]
foreign import playerName :: String
-- statistics = [stat1,stat2]

stat1 = { matches: 20
        , label: "Last 20"
        , winPercentage: "20"
        , avgDuration: "10:24"
        , maxBreak: "99"
        , avgMax: "33"
        , ranking: "5300"
        , rankDiff: "30.99"
        , breakStats: [ { minimum: "10"
                        , percent: "93.0"
                        , absolute: "19"
                        , wPercentage: "90.0"
                        }
                      , { minimum: "20"
                        , percent: "88.0"
                        , absolute: "17"
                        , wPercentage: "80.0"
                        }
                      , { minimum: "30"
                        , percent: "73.0"
                        , absolute: "13"
                        , wPercentage: "66.0"
                        }
                      ]
      }

stat2 = { matches: 40
        , label: "Last 40"
        , winPercentage: "80"
        , avgDuration: "15:24"
        , maxBreak: "44"
        , avgMax: "13"
        , ranking: "5100"
        , rankDiff: "10.99"
        , breakStats: [ { minimum: "10"
                        , percent: "73.0"
                        , absolute: "25"
                        , wPercentage: "80.0"
                        }
                      , { minimum: "18"
                        , percent: "81.0"
                        , absolute: "14"
                        , wPercentage: "70.0"
                        }
                      , { minimum: "30"
                        , percent: "53.0"
                        , absolute: "11"
                        , wPercentage: "56.0"
                        }
                      ]
      }

           

stats = M.fromList $ (\pbs -> Tuple pbs.label pbs) <$> statistics

data State = State Statistics String

-- | Inputs to the state machine
data Input = Select String

ui :: forall m eff. (Applicative m, Control.Alternative.Alternative m) => Component m Input Input
ui = render <$> stateful (State (snd firstState) (fst firstState))  update
  where
  firstState = AU.head $ M.toList stats
  class' clazz = A.class_ (A.className clazz)
  stat :: String -> String -> H.HTML (m Input)
  stat key value = H.div [ class' "stat" ]
                         [ H.span [ class' "left" ]  [ H.text key ]
                         , H.span [ class' "right" ] [ H.text value ]
                         ]
  render :: State -> H.HTML (m Input)
  render (State s label)  = H.div_ [ H.div [ class' "playerInfo" ]
            [

              H.select [ A.onValueChanged (A.input Select ) ] (flip (<$>) (M.toList stats) (\ (Tuple label info) -> H.option [ A.name label ] [ H.text label ]))
            , stat "Matches: " (show s.matches)
            , stat "Win %: " s.winPercentage
            , stat "Average Duration: " s.avgDuration
            , stat "Max Break:" s.maxBreak
            , stat "Average Max:" s.avgMax
            , stat "Ranking" s.ranking
            , stat "Ranking difference" s.rankDiff
            ]
    , H.table [ class' "breaksTable" ]
            [ H.thead_ [ H.tr_ [ H.th_ [ H.text "Break >=" ]
                               , H.th_ [ H.text "Prozent" ]
                               , H.th_ [ H.text "Absolut" ]
                               , H.th_ [ H.text "Win %" ]]]
            , H.tbody_ (renderBreak <$> s.breakStats)]
    , H.div [ class' "rankingBox" ] [ H.img [ A.src $ label ++ "-" ++ playerName ++ ".png" ] [] ] 
    , H.div [ class' "matchesBox" ] [H.table [ class' "matchesTable" ]
                                         [ H.tbody_ (renderMatches $ take s.matches matchesOfPlayer) ]]
    
                        
    ]    


  niceDate :: { da :: DayOfMonth, mo :: Month, ye :: Year } -> String
  niceDate d = shD d.da ++ ". " ++ show d.mo ++ " " ++ shY d.ye
    where
    shD (DayOfMonth d) = padded <<< toNumber $ d
    shY (Year y) = padded <<< toNumber $ y

  renderMatches matches = flip (<$>) (dayToMatches matches) $ \(Tuple (MyDate d) ms) -> 
    H.div [ class' "matchesOfDay" ]
          [ H.div_ [ H.h3_ [H.text $ niceDate d] ]
          , H.table [ class' "matchesTable" ]
                    [ H.tbody_ (renderMatch <$> ms) ]
          ]

  showTime m = pad1 (hourOfDay d) ++ ":" ++ pad2 (minuteOfHour d)
    where
      d = toDate m
      pad2 (MinuteOfHour m) = padded <<< toNumber $  m
      pad1 (HourOfDay h) = padded <<< toNumber $ h


  markupPlayer player winner = if player == winner then H.b_ [ H.text player ] else H.text player
  renderMatch m = H.tr_ [ H.td [ class' "matchDate" ] [ H.text $ showTime m ]
                        , H.td [ class' "player1" ] [ markupPlayer m.player1 m.winner ]
                        , H.td [ class' "player1" ] [ H.text m.ranking1 ]
                        , H.td [ class' "player1" ] [ H.text m.maxBreak1 ]
                        , H.td [ class' "player2" ] [ markupPlayer m.player2 m.winner ]
                        , H.td [ class' "player2" ] [ H.text m.ranking2 ]
                        , H.td [ class' "player2" ] [ H.text m.maxBreak2 ]
                        ]
  renderBreak b = H.tr_ [ H.td_ [ H.text b.minimum ]
                        , H.td_ [ H.text b.percent ]
                        , H.td_ [ H.text b.absolute ]
                        , H.td_ [ H.text b.wPercentage ]]
      
  update :: State -> Input -> State
  update (State s label') (Select label) = State (fromJust (M.lookup label stats)) label

padded n | n < 10 = "0" ++ show n
padded n = show n

data MyDate = MyDate { ye :: Year, mo :: Month, da :: DayOfMonth }
instance showMyDate :: Eq MyDate where
   (==) (MyDate d) (MyDate d') = d.ye == d'.ye && d.mo == d'.mo && d.da == d'.da
   (/=) d d' = not $ d == d'

instance showDayOfMonth :: Show DayOfMonth where
   show (DayOfMonth d) = show d

dayToMatches :: [Match] -> [Tuple MyDate [Match]]
dayToMatches matches =  (first AU.head <<< unzip) <$> ((groupBy ((==) `on` fst)) $ (\m -> Tuple (toDateAbout m) m) <$> matches)
  where
   toDateAbout m = MyDate { ye: year d, mo: month d, da: dayOfMonth d }
     where
     d = toDate m

toDate m = fromJust $ fromString m.date


main = do
  Tuple node _ <- runUI ui
  appendToWrapper node
