module Container where

import Prelude

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import ComponentA as CA
import ComponentB as CB
import ComponentC as CC

type State =
  { a :: Maybe Boolean
  , b :: Maybe Int
  , c :: Maybe String
  }

data Query a = ReadStates a

type ChildQuery = Coproduct3 CA.Query CB.Query CC.Query

type ChildSlot = Either3 Unit Unit Unit

component :: forall m. Applicative m => H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { a: Nothing, b: Nothing, c: Nothing }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state = HH.div_
    [ HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h1_ [ HH.text "Component A" ]
        , HH.slot' CP.cp1 unit CA.component unit absurd
        ]
    , HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h1_ [ HH.text "Component B" ]
        , HH.slot' CP.cp2 unit CB.component unit absurd
        ]
    , HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h1_ [ HH.text "Component C" ]
        , HH.slot' CP.cp3 unit CC.component unit absurd
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

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (ReadStates next) = do
    a <- H.query' CP.cp1 unit (H.request CA.GetState)
    b <- H.query' CP.cp2 unit (H.request CB.GetCount)
    c <- H.query' CP.cp3 unit (H.request CC.GetValue)
    H.put { a, b, c }
    pure next
