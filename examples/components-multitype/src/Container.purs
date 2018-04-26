module Container where

import Prelude

import ComponentA as CA
import ComponentB as CB
import ComponentC as CC
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { a :: Maybe Boolean
  , b :: Maybe Int
  , c :: Maybe String
  }

data Query a = ReadStates a

type ChildSlots =
  ( a :: CA.Slot Unit
  , b :: CB.Slot Unit
  , c :: CC.Slot Unit
  )

_a = SProxy :: SProxy "a"
_b = SProxy :: SProxy "b"
_c = SProxy :: SProxy "c"

component :: forall m. Applicative m => H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where

  initialState :: State
  initialState = { a: Nothing, b: Nothing, c: Nothing }

  render :: State -> H.ComponentHTML Query ChildSlots m
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
        [ HE.onClick (HE.input_ ReadStates) ]
        [ HH.text "Check states now" ]
    ]

  eval :: Query ~> H.HalogenM State Query ChildSlots Void m
  eval (ReadStates next) = do
    a <- H.query _a unit (H.request CA.GetState)
    b <- H.query _b unit (H.request CB.GetCount)
    c <- H.query _c unit (H.request CC.GetValue)
    H.put { a, b, c }
    pure next
