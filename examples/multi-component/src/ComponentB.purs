module Example.ComponentB where

import Prelude

import Data.Generic (Generic, gEq, gCompare)

import Halogen
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events as E

newtype StateB = StateB { on :: Boolean }

initStateB = StateB { on: false }

data InputB a
  = ToggleStateB a
  | GetStateB (Boolean -> a)

data SlotB = SlotB

derive instance genericSlotB :: Generic SlotB
instance eqSlotB :: Eq SlotB where eq = gEq
instance ordSlotB :: Ord SlotB where compare = gCompare

componentB :: forall g p. (Functor g) => Component StateB InputB g p
componentB = component render eval
  where

  render :: Render StateB InputB p
  render (StateB state) = H.div_
    [ H.h1_ [ H.text "Toggle Button B" ]
    , H.button [ E.onClick (E.input_ ToggleStateB) ]
               [ H.text (if state.on then "On" else "Off") ]
    ]

  eval :: Eval InputB StateB InputB g
  eval (ToggleStateB next) = do
    modify (\(StateB state) -> StateB { on: not state.on })
    pure next
  eval (GetStateB continue) = do
    b <- gets (\(StateB state) -> state.on)
    pure (continue b)
