module Example.ComponentC where

import Prelude

import Data.Generic (Generic, gEq, gCompare)

import Halogen
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events as E

newtype StateC = StateC { on :: Boolean }

initStateC = StateC { on: false }

data InputC a
  = ToggleStateC a
  | GetStateC (Boolean -> a)

data SlotC = SlotC

derive instance genericSlotC :: Generic SlotC
instance eqSlotC :: Eq SlotC where eq = gEq
instance ordSlotC :: Ord SlotC where compare = gCompare

componentC :: forall g p. (Functor g) => Component StateC InputC g p
componentC = component render eval
  where

  render :: Render StateC InputC p
  render (StateC state) = H.div_
    [ H.h1_ [ H.text "Toggle Button C" ]
    , H.button [ E.onClick (E.input_ ToggleStateC) ]
               [ H.text (if state.on then "On" else "Off") ]
    ]

  eval :: Eval InputC StateC InputC g
  eval (ToggleStateC next) = do
    modify (\(StateC state) -> StateC { on: not state.on })
    pure next
  eval (GetStateC continue) = do
    b <- gets (\(StateC state) -> state.on)
    pure (continue b)
