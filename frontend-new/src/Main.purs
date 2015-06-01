module Main where

import Data.Void
import Data.Tuple
import Data.Either
import Data.Maybe.Unsafe
import Data.Maybe

import Control.Bind
import Control.Monad.Eff

import DOM

import qualified Data.Map as M       
import Data.Foldable       
import Data.Traversable

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A

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
                  , days          :: [Day]
                  }

type Break = { minimum       :: String
             , percent       :: String
             , absolute      :: String
             , wPercentage :: String
             }

type Day = { day            :: String
           , matches        :: [Match]
           }

type Match = { duration       :: String
             , player1        :: String
             , ranking1       :: String
             , break1         :: String
             , player2        :: String
             , ranking2       :: String
             , break2         :: String
             }

foreign import statistics :: [Statistics]
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
        , days: [ { day: "30.05.2015"
                  , matches: [ { duration: "20:03"
                               , player1: "herbyger"
                               , ranking1: "5200"
                               , break1: "44"
                               , player2: "momsia"
                               , ranking2: "5400"
                               , break2: "33"
                               }
                             ]
                  }
                , { day: "28.05.2015"
                  , matches: [ { duration: "10:03"
                               , player1: "p.hanpe"
                               , ranking1: "5800"
                               , break1: "94"
                               , player2: "whatsacue4"
                               , ranking2: "5400"
                               , break2: "43"
                               }
                             ]
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
        , days: [ { day: "29.05.2015"
                  , matches: [ { duration: "20:03"
                               , player1: "herbyger"
                               , ranking1: "5200"
                               , break1: "44"
                               , player2: "momsia"
                               , ranking2: "5400"
                               , break2: "33"
                               }
                             ]
                  }
                , { day: "27.05.2015"
                  , matches: [ { duration: "10:03"
                               , player1: "p.hanpe"
                               , ranking1: "5800"
                               , break1: "94"
                               , player2: "whatsacue4"
                               , ranking2: "5400"
                               , break2: "43"
                               }
                             ]
                  }
                ]
      }

           

stats = M.fromList $ (\pbs -> Tuple pbs.label pbs) <$> statistics

-- [ Tuple "20" stat1, Tuple "40" stat2 ]

-- | The state of the application
-- newtype State = State { on :: Boolean, stateStat :: String }

-- | Inputs to the state machine
data Input = ToggleState | Select String

ui :: forall m eff. (Applicative m, Control.Alternative.Alternative m) => Component m Input Input
ui = render <$> stateful stat1 update
  where
  class' clazz = A.class_ (A.className clazz)
  stat :: String -> String -> H.HTML (m Input)
  stat key value = H.div [ class' "stat" ]
                         [ H.span [ class' "left" ]  [ H.text key ]
                         , H.span [ class' "right" ] [ H.text value ]
                         ]
  render :: Statistics -> H.HTML (m Input)
  render s = H.div_
    [ H.div [ class' "playerInfo" ]
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
  --  , H.div [ class' "matchesBox" ] (renderMatchesOfDay <$> s.days)
    
                        
    ]    
  renderMatchesOfDay d = H.div [ class' "matchesOfDay" ] 
                               [ H.div_ [ H.h3_ [H.text d.day] ]
                               , H.table [ class' "matchesTable" ]
                                         [ H.tbody_ (renderMatch <$> d.matches) ]
                               ]
  renderMatch m = H.tr_ [ H.td [ class' "matchDate" ] [ H.text m.duration ]
                        , H.td [ class' "player1" ] [ H.text m.player1 ]
                        , H.td [ class' "player1" ] [ H.text m.ranking1 ]
                        , H.td [ class' "player1" ] [ H.text m.break1 ]
                        , H.td [ class' "player2" ] [ H.text m.player2 ]
                        , H.td [ class' "player2" ] [ H.text m.ranking2 ]
                        , H.td [ class' "player2" ] [ H.text m.break2 ]
                        ]
  renderBreak b = H.tr_ [ H.td_ [ H.text b.minimum ]
                        , H.td_ [ H.text b.percent ]
                        , H.td_ [ H.text b.absolute ]
                        , H.td_ [ H.text b.wPercentage ]]
      
  update :: Statistics -> Input -> Statistics
  update s (Select label) = fromJust (M.lookup label stats)

main = do
  Tuple node _ <- runUI ui
  appendToWrapper node
