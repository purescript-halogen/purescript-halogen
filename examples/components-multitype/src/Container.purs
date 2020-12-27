module Example.Components.Multitype.Container where

import Prelude

import Data.Maybe (Maybe(..))
import Example.Components.Multitype.ComponentA as CA
import Example.Components.Multitype.ComponentB as CB
import Example.Components.Multitype.ComponentC as CC
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

data Action = ReadStates

type State =
  { a :: Maybe Boolean
  , b :: Maybe Int
  , c :: Maybe String
  }

type ChildSlots =
  ( a :: CA.Slot Unit
  , b :: CB.Slot Unit
  , c :: CC.Slot Unit
  )

_a = Proxy :: Proxy "a"
_b = Proxy :: Proxy "b"
_c = Proxy :: Proxy "c"

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall i. i -> State
initialState _ = { a: Nothing, b: Nothing, c: Nothing }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state = HH.div_
  [ HH.div
      [ HP.class_ (H.ClassName "box")]
      [ HH.h1_ [ HH.text "Component A" ]
      , HH.slot_ _a unit CA.component unit
      ]
  , HH.div
      [ HP.class_ (H.ClassName "box")]
      [ HH.h1_ [ HH.text "Component B" ]
      , HH.slot_ _b unit CB.component unit
      ]
  , HH.div
      [ HP.class_ (H.ClassName "box")]
      [ HH.h1_ [ HH.text "Component C" ]
      , HH.slot_ _c unit CC.component unit
      ]
  , HH.p_
      [ HH.text "Last observed states:"]
  , HH.ul_
      [ HH.li_ [ HH.text ("Component A: " <> show state.a) ]
      , HH.li_ [ HH.text ("Component B: " <> show state.b) ]
      , HH.li_ [ HH.text ("Component C: " <> show state.c) ]
      ]
  , HH.button
      [ HE.onClick \_ -> ReadStates ]
      [ HH.text "Check states now" ]
  ]

handleAction :: forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  ReadStates -> do
    a <- H.request _a unit CA.IsOn
    b <- H.request _b unit CB.GetCount
    c <- H.request _c unit CC.GetValue
    H.put { a, b, c }
