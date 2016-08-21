module Ticker where

import Prelude

import Control.Monad.Free (Free)

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE

type TickState = Int

data TickQuery a
  = Tick a
  | GetTick (Int -> a)

ticker :: forall g. Int -> H.Component TickQuery g
ticker n = H.component { render, eval, initialState: n }
  where
  render :: TickState -> H.ComponentHTML TickQuery g
  render state =
    HH.div
      [ HP.class_ (HH.className "ticker") ]
      [ HH.h1
          [ HP.id_ "header" ]
          [ HH.text "counter" ]
      , HH.p_
          [ HH.text (show state) ]
      , HH.button
          [ HE.onClick (HE.input_ Tick) ]
          [ HH.text "Tick" ]
      ]

  eval :: TickQuery ~> Free (H.ComponentDSL TickState TickQuery g)
  eval (Tick next) = do
    H.modify (_ + 1)
    pure next
  eval (GetTick continue) = do
    n <- H.get
    pure (continue n)
