module Example.Components.Multitype.ComponentB where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Slot = H.Slot Query Void

data Query a = GetCount (Int -> a)

data Action = Increment

type State = Int

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
initialState _ = 0

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.p_
        [ HH.text "Current value:"
        , HH.strong_ [ HH.text (show state) ]
        ]
    , HH.button
        [ HE.onClick \_ -> Increment ]
        [ HH.text ("Increment") ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Increment ->
    H.modify_ (_ + 1)

handleQuery :: forall o m a. Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  GetCount k ->
    Just <<< k <$> H.get
