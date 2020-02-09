module Example.Components.Multitype.ComponentC where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slot = H.Slot Query Void

data Query a = GetValue (String -> a)

data Action = HandleInput String

type State = String

component :: forall i o m. H.Component Query i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

initialState :: forall i. i -> State
initialState _ = "Hello"

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.label_
    [ HH.p_ [ HH.text "What do you have to say?" ]
    , HH.input
        [ HP.value state
        , HE.onValueInput HandleInput
        ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleInput value ->
    H.put value

handleQuery :: forall o m a. Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  GetValue k ->
    Just <<< k <$> H.get
