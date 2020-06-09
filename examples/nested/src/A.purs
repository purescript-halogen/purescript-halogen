module Example.Nested.A (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Example.Nested.B as B
import Example.Nested.C as C
import Data.Const
import Data.Symbol (SProxy(..))

type State = Unit

data Action = Void

type ChildSlots =
  ( b :: H.Slot (Const Void) Void Unit
  , c :: H.Slot (Const Void) Void Unit
  )

_b :: SProxy "b"
_b = SProxy

_c :: SProxy "c"
_c = SProxy

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
    [ HH.li_ [ HH.text "a" ]
    , HH.li_ [ HH.slot _b unit B.component unit absurd ]
    , HH.li_ [ HH.slot _c unit C.component unit absurd ]
    , HH.li_ [ HH.text "a end" ]
    ]
