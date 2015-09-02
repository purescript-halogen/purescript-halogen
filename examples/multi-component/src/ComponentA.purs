module Example.ComponentA where

import Prelude

import Data.Functor (($>))
import Data.Generic (Generic, gEq, gCompare)

import Halogen
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events as E

newtype StateA = StateA { on :: Boolean }

initStateA = StateA { on: false }

data InputA a
  = ToggleStateA a
  | GetStateA (Boolean -> a)

data PlaceholderA = PlaceholderA

derive instance genericPlaceholderA :: Generic PlaceholderA
instance eqPlaceholderA :: Eq PlaceholderA where eq = gEq
instance ordPlaceholderA :: Ord PlaceholderA where compare = gCompare

componentA :: forall g p. (Functor g) => Component StateA InputA g p
componentA = component render eval
  where

  render :: Render StateA InputA p
  render (StateA state) = H.div_
    [ H.h1_ [ H.text "Toggle Button A" ]
    , H.button [ E.onClick (E.input_ ToggleStateA) ]
               [ H.text (if state.on then "On" else "Off") ]
    ]

  eval :: Eval InputA StateA InputA g
  eval (ToggleStateA next) = modify (\(StateA state) -> StateA { on: not state.on }) $> next
  eval (GetStateA k) = gets (\(StateA state) -> state.on) >>= pure <<< k
