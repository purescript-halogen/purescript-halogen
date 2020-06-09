module Example.Nested.B (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Example.Nested.D as D
import Example.Nested.E as E
import Data.Const
import Data.Symbol (SProxy(..))

type State = Unit

data Action = Void

type ChildSlots =
  ( d :: H.Slot (Const Void) Void Unit
  , e :: H.Slot (Const Void) Void Unit
  )

_d :: SProxy "d"
_d = SProxy

_e :: SProxy "e"
_e = SProxy

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
    [ HH.li_ [ HH.text "b" ]
    , HH.li_ [ HH.slot _d unit D.component unit absurd ]
    , HH.li_ [ HH.slot _e unit E.component unit absurd ]
    , HH.li_ [ HH.text "b end" ]
    ]
