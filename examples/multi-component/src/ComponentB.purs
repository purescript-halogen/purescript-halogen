module ComponentB where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE

newtype StateB = StateB { on :: Boolean }

initStateB :: StateB
initStateB = StateB { on: false }

data QueryB a
  = ToggleStateB a
  | GetStateB (Boolean -> a)

data SlotB = SlotB
derive instance eqSlotB :: Eq SlotB
derive instance ordSlotB :: Ord SlotB

componentB :: forall g. H.Component StateB QueryB g
componentB = H.component { render, eval }
  where

  render :: StateB -> H.ComponentHTML QueryB
  render (StateB state) = HH.div_
    [ HH.h1_ [ HH.text "Toggle Button B" ]
    , HH.button
        [ HE.onClick (HE.input_ ToggleStateB) ]
        [ HH.text (if state.on then "On" else "Off") ]
    ]

  eval :: QueryB ~> H.ComponentDSL StateB QueryB g
  eval (ToggleStateB next) = do
    H.modify (\(StateB state) -> StateB { on: not state.on })
    pure next
  eval (GetStateB continue) = do
    b <- H.gets (\(StateB state) -> state.on)
    pure (continue b)
