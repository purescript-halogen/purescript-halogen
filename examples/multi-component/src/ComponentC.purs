module ComponentC where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

newtype StateC = StateC { on :: Boolean }

initialState :: StateC
initialState = StateC { on: false }

data QueryC a
  = ToggleStateC a
  | GetStateC (Boolean -> a)

data SlotC = SlotC
derive instance eqSlotC :: Eq SlotC
derive instance ordSlotC :: Ord SlotC

componentC :: forall m. H.Component HH.HTML QueryC Void m
componentC = H.component { render, eval, initialState }
  where

  render :: StateC -> H.ComponentHTML QueryC
  render (StateC state) = HH.div_
    [ HH.h1_ [ HH.text "Toggle Button C" ]
    , HH.button
        [ HE.onClick (HE.input_ ToggleStateC) ]
        [ HH.text (if state.on then "On" else "Off") ]
    ]

  eval :: QueryC ~> H.ComponentDSL StateC QueryC Void m
  eval (ToggleStateC next) = do
    H.modify (\(StateC state) -> StateC { on: not state.on })
    pure next
  eval (GetStateC reply) = do
    b <- H.gets (\(StateC state) -> state.on)
    pure (reply b)
