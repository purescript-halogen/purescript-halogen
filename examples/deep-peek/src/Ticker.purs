module Ticker where

import Prelude

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E

data TickState = TickState Int

data TickQuery a
  = Tick a
  | GetTick (Int -> a)

tickerComponent :: forall g. (Functor g) => Component TickState TickQuery g
tickerComponent = component { render, eval }
  where

  render :: TickState -> ComponentHTML TickQuery
  render (TickState n) =
    H.div_
      [ H.h1
          [ P.id_ "header" ]
          [ H.text "counter" ]
      , H.p_
          [ H.text (show n) ]
      , H.button
          [ E.onClick (E.input_ Tick) ]
          [ H.text "Tick" ]
      ]

  eval :: TickQuery ~> (ComponentDSL TickState TickQuery g)
  eval (Tick next) = do
    modify (\(TickState n) -> TickState (n + 1))
    pure next
  eval (GetTick continue) = do
    TickState n <- get
    pure (continue n)
