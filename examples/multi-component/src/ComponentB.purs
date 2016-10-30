module ComponentB where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

newtype StateB = StateB { on :: Boolean }

initialState :: StateB
initialState = StateB { on: false }

data QueryB a
  = ToggleStateB a
  | GetStateB (Boolean -> a)

data SlotB = SlotB
derive instance eqSlotB :: Eq SlotB
derive instance ordSlotB :: Ord SlotB

componentB :: forall m. H.Component HH.HTML QueryB Void m
componentB = H.component { render, eval, initialState }
  where

  render :: StateB -> H.ComponentHTML QueryB
  render (StateB state) = HH.div_
    [ HH.h1_ [ HH.text "Toggle Button B" ]
    , HH.button
        [ HE.onClick (HE.input_ ToggleStateB) ]
        [ HH.text (if state.on then "On" else "Off") ]
    ]

  eval :: QueryB ~> H.ComponentDSL StateB QueryB Void m
  eval (ToggleStateB next) = do
    H.modify (\(StateB state) -> StateB { on: not state.on })
    pure next
  eval (GetStateB continue) = do
    b <- H.gets (\(StateB state) -> state.on)
    pure (continue b)
