module Ticker where

import Prelude

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

type TickState = Int

data TickQuery a
  = Tick a
  | GetTick (Int -> a)

tickerComponent :: forall g. H.Component TickState TickQuery g
tickerComponent = H.component { render, eval }
  where

  render :: TickState -> H.ComponentHTML TickQuery
  render n =
    HH.div_
      [ HH.h1
          [ HP.id_ "header" ]
          [ HH.text "counter" ]
      , HH.p_
          [ HH.text (show n) ]
      , HH.button
          [ HE.onClick (HE.input_ Tick) ]
          [ HH.text "Tick" ]
      ]

  eval :: TickQuery ~> H.ComponentDSL TickState TickQuery g
  eval (Tick next) = do
    H.modify (_ + 1)
    pure next
  eval (GetTick continue) = do
    n <- H.get
    pure (continue n)
