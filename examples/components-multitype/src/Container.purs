module Example.Components.Multitype.Container where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Example.Components.Multitype.ComponentA as CA
import Example.Components.Multitype.ComponentB as CB
import Example.Components.Multitype.ComponentC as CC
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

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

_a = SProxy :: SProxy "a"
_b = SProxy :: SProxy "b"
_c = SProxy :: SProxy "c"

component :: forall f i o m. H.Component HH.HTML f i o m
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
      , HH.slot _a unit CA.component unit absurd
      ]
  , HH.div
      [ HP.class_ (H.ClassName "box")]
      [ HH.h1_ [ HH.text "Component B" ]
      , HH.slot _b unit CB.component unit absurd
      ]
  , HH.div
      [ HP.class_ (H.ClassName "box")]
      [ HH.h1_ [ HH.text "Component C" ]
      , HH.slot _c unit CC.component unit absurd
      ]
  , HH.p_
      [ HH.text "Last observed states:"]
  , HH.ul_
      [ HH.li_ [ HH.text ("Component A: " <> show state.a) ]
      , HH.li_ [ HH.text ("Component B: " <> show state.b) ]
      , HH.li_ [ HH.text ("Component C: " <> show state.c) ]
      ]
  , HH.button
      [ HE.onClick (\_ -> Just ReadStates) ]
      [ HH.text "Check states now" ]
  ]

handleAction :: forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  ReadStates -> do
    a <- H.query _a unit (H.request CA.IsOn)
    b <- H.query _b unit (H.request CB.GetCount)
    c <- H.query _c unit (H.request CC.GetValue)
    H.put { a, b, c }
