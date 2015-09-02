module Example.ComponentC where

import Prelude

import Data.Functor (($>))
import Data.Generic (Generic, gEq, gCompare)

import Halogen
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events as E

newtype StateC = StateC { on :: Boolean }

initStateC = StateC { on: false }

data InputC a
  = ToggleStateC a
  | GetStateC (Boolean -> a)

data PlaceholderC = PlaceholderC

derive instance genericPlaceholderC :: Generic PlaceholderC
instance eqPlaceholderC :: Eq PlaceholderC where eq = gEq
instance ordPlaceholderC :: Ord PlaceholderC where compare = gCompare

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
  eval (ToggleStateC next) = modify (\(StateC state) -> StateC { on: not state.on }) $> next
  eval (GetStateC k) = gets (\(StateC state) -> state.on) >>= pure <<< k
