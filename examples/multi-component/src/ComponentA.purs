module ComponentA where

import Prelude

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E

newtype StateA = StateA { on :: Boolean }

initStateA :: StateA
initStateA = StateA { on: false }

data QueryA a
  = ToggleStateA a
  | GetStateA (Boolean -> a)

data SlotA = SlotA
derive instance eqSlotA :: Eq SlotA
derive instance ordSlotA :: Ord SlotA

componentA :: forall g. (Functor g) => Component StateA QueryA g
componentA = component { render, eval }
  where

  render :: StateA -> ComponentHTML QueryA
  render (StateA state) = H.div_
    [ H.h1_ [ H.text "Toggle Button A" ]
    , H.button [ E.onClick (E.input_ ToggleStateA) ]
               [ H.text (if state.on then "On" else "Off") ]
    ]

  eval :: Natural QueryA (ComponentDSL StateA QueryA g)
  eval (ToggleStateA next) = do
    modify (\(StateA state) -> StateA { on: not state.on })
    pure next
  eval (GetStateA continue) = do
    b <- gets (\(StateA state) -> state.on)
    pure (continue b)
