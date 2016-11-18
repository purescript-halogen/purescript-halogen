module Ticker where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type TickState = Int

data TickQuery a
  = Tick a
  | GetTick (Int -> a)

ticker :: forall m. Int -> H.Component HH.HTML TickQuery Void m
ticker initialState = H.component { render, eval, initialState }
  where
  render :: TickState -> H.ComponentHTML TickQuery
  render state =
    HH.div
      [ HP.class_ (HH.ClassName "ticker") ]
      [ HH.h1
          [ HP.id_ "header" ]
          [ HH.text "counter" ]
      , HH.p_
          [ HH.text (show state) ]
      , HH.button
          [ HE.onClick (HE.input_ Tick) ]
          [ HH.text "Tick" ]
      ]

  eval :: TickQuery ~> H.ComponentDSL TickState TickQuery Void m
  eval (Tick next) = do
    H.modify (_ + 1)
    pure next
  eval (GetTick continue) = do
    n <- H.get
    pure (continue n)
