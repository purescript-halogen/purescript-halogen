module Ticker where

import Prelude

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E

type TickState = Int

data TickQuery a
  = Tick a
  | GetTick (Int -> a)

ticker :: forall g. Component TickState TickQuery g
ticker = component { render, eval }
  where

  render :: TickState -> ComponentHTML TickQuery
  render state =
    H.div_
      [ H.h1
          [ P.id_ "header" ]
          [ H.text "counter" ]
      , H.p_
          [ H.text (show state) ]
      , H.button
          [ E.onClick (E.input_ Tick) ]
          [ H.text "Tick" ]
      ]

  eval :: TickQuery ~> (ComponentDSL TickState TickQuery g)
  eval (Tick next) = do
    modify (_ + 1)
    pure next
  eval (GetTick continue) = do
    n <- get
    pure (continue n)
