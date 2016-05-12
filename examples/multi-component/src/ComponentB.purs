module ComponentB where

import Prelude

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E

newtype StateB = StateB { on :: Boolean }

initStateB :: StateB
initStateB = StateB { on: false }

data QueryB a
  = ToggleStateB a
  | GetStateB (Boolean -> a)

data SlotB = SlotB
derive instance eqSlotB :: Eq SlotB
derive instance ordSlotB :: Ord SlotB

componentB :: forall g. (Functor g) => Component StateB QueryB g
componentB = component { render, eval }
  where

  render :: StateB -> ComponentHTML QueryB
  render (StateB state) = H.div_
    [ H.h1_ [ H.text "Toggle Button B" ]
    , H.button [ E.onClick (E.input_ ToggleStateB) ]
               [ H.text (if state.on then "On" else "Off") ]
    ]

  eval :: Natural QueryB (ComponentDSL StateB QueryB g)
  eval (ToggleStateB next) = do
    modify (\(StateB state) -> StateB { on: not state.on })
    pure next
  eval (GetStateB continue) = do
    b <- gets (\(StateB state) -> state.on)
    pure (continue b)
