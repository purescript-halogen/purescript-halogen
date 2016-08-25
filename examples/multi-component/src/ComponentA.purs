module ComponentA where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Events.Indexed as HE

newtype StateA = StateA { on :: Boolean }

initialState :: StateA
initialState = StateA { on: false }

data QueryA a
  = ToggleStateA a
  | GetStateA (Boolean -> a)

data SlotA = SlotA
derive instance eqSlotA :: Eq SlotA
derive instance ordSlotA :: Ord SlotA

componentA :: forall g. H.Component QueryA g
componentA = H.component { render, eval, initialState }
  where

  render :: StateA -> H.ComponentHTML QueryA g
  render (StateA state) = HH.div_
    [ HH.h1_ [ HH.text "Toggle Button A" ]
    , HH.button
        [ HE.onClick (HE.input_ ToggleStateA) ]
        [ HH.text (if state.on then "On" else "Off") ]
    ]

  eval :: QueryA ~> H.ComponentDSL StateA QueryA g
  eval (ToggleStateA next) = do
    H.modify (\(StateA state) -> StateA { on: not state.on })
    pure next
  eval (GetStateA continue) = do
    b <- H.gets (\(StateA state) -> state.on)
    pure (continue b)
