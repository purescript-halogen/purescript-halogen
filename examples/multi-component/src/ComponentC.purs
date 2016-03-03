module ComponentC where

import Prelude

import Data.Generic (Generic, gEq, gCompare)

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E

newtype StateC = StateC { on :: Boolean }

initStateC :: StateC
initStateC = StateC { on: false }

data QueryC a
  = ToggleStateC a
  | GetStateC (Boolean -> a)

data SlotC = SlotC

derive instance genericSlotC :: Generic SlotC
instance eqSlotC :: Eq SlotC where eq = gEq
instance ordSlotC :: Ord SlotC where compare = gCompare

componentC :: forall g. (Functor g) => Component StateC QueryC g
componentC = component { render, eval }
  where

  render :: StateC -> ComponentHTML QueryC
  render (StateC state) = H.div_
    [ H.h1_ [ H.text "Toggle Button C" ]
    , H.button [ E.onClick (E.input_ ToggleStateC) ]
               [ H.text (if state.on then "On" else "Off") ]
    ]

  eval :: Natural QueryC (ComponentDSL StateC QueryC g)
  eval (ToggleStateC next) = do
    modify (\(StateC state) -> StateC { on: not state.on })
    pure next
  eval (GetStateC continue) = do
    b <- gets (\(StateC state) -> state.on)
    pure (continue b)
