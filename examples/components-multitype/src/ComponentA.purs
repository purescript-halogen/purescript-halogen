module Example.Components.Multitype.ComponentA where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type Slot = H.Slot Query Void

data Query a = IsOn (Boolean -> a)

data Action = Toggle

type State = Boolean

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
initialState _ = false

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.p_ [ HH.text "Toggle me!" ]
    , HH.button
        [ HE.onClick \_ -> Toggle ]
        [ HH.text (if state then "On" else "Off") ]
    ]

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Toggle ->
    H.modify_ not

handleQuery :: forall o m a. Query a -> H.HalogenM State Action () o m (Maybe a)
handleQuery = case _ of
  IsOn k -> do
    enabled <- H.get
    pure (Just (k enabled))
