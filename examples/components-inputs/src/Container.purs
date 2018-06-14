module Example.Components.Inputs.Container where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Example.Components.Inputs.Display as Display
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a
  = Increment a
  | Decrement a

type State = Int

type ChildSlots =
  ( display :: Display.Slot Int
  )

_display = SProxy :: SProxy "display"

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where
  initialState :: State
  initialState = 1

  render :: State -> H.ComponentHTML Query ChildSlots m
  render state =
    HH.div_
      [ HH.ul_
          [ HH.slot _display 1 Display.component state absurd
          , HH.slot _display 2 Display.component (state * 2) absurd
          , HH.slot _display 3 Display.component (state * 3) absurd
          , HH.slot _display 4 Display.component (state * 10) absurd
          , HH.slot _display 5 Display.component (state * state) absurd
          ]
      , HH.button
          [ HE.onClick (HE.input_ Increment) ]
          [ HH.text "+1"]
      , HH.button
          [ HE.onClick (HE.input_ Decrement) ]
          [ HH.text "-1"]
      ]

  eval :: Query ~> H.HalogenM State Query ChildSlots Void m
  eval = case _ of
    Increment next -> do
      H.modify_ (_ + 1)
      pure next
    Decrement next -> do
      H.modify_ (_ - 1)
      pure next
