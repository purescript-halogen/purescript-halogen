module Example.Nested.C (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Example.Nested.D as D
import Example.Nested.E as E

type State = Unit

data Action = Void

type ChildSlots = ()

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval H.defaultEval
    }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.ul_
    [ HH.li_ [ HH.text "c" ]
    ]
