module ComponentC where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE

newtype StateC = StateC { on :: Boolean }

initialState :: StateC
initialState = StateC { on: false }

data QueryC a
  = ToggleStateC a
  | GetStateC (Boolean -> a)

data SlotC = SlotC
derive instance eqSlotC :: Eq SlotC
derive instance ordSlotC :: Ord SlotC

componentC :: forall g. H.Component QueryC g
componentC = H.component { render, eval, initialState }
  where

  render :: StateC -> H.ComponentHTML QueryC g
  render (StateC state) = HH.div_
    [ HH.h1_ [ HH.text "Toggle Button C" ]
    , HH.button
        [ HE.onClick (HE.input_ ToggleStateC) ]
        [ HH.text (if state.on then "On" else "Off") ]
    ]

  eval :: QueryC ~> H.ComponentDSL StateC QueryC g
  eval (ToggleStateC next) = do
    H.modify (\(StateC state) -> StateC { on: not state.on })
    pure next
  eval (GetStateC continue) = do
    b <- H.gets (\(StateC state) -> state.on)
    pure (continue b)
