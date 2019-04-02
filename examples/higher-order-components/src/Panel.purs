module Example.HOC.Panel (Slot, Query(..), Message(..), component) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slot f o = H.Slot (Query f) (Message o)

data Query f a
  = SetOpen Boolean a
  | QueryInner (f a)

data Message o
  = Bubble o
  | Opened
  | Closed

data Action o
  = Toggle
  | HandleInner o

type State i =
  { input :: i
  , open :: Boolean
  }

type ChildSlots f o =
  ( inner :: H.Slot f o Unit
  )

_inner :: SProxy "inner"
_inner = SProxy

component
  :: forall f i o m
   . H.Component HH.HTML f i o m
  -> H.Component HH.HTML (Query f) i (Message o) m
component innerComponent =
  H.mkComponent
    { initialState
    , render: render innerComponent
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

initialState :: forall i. i -> State i
initialState = { input: _, open: false }

render
  :: forall f i o m
   . H.Component HH.HTML f i o m
  -> State i
  -> H.ComponentHTML (Action o) (ChildSlots f o) m
render innerComponent state
  | state.open =
      HH.div
        [ HP.classes [ H.ClassName "Panel", H.ClassName "Panel--open" ] ]
        [ HH.div
            [ HP.classes [ H.ClassName "Panel-header" ] ]
            [ HH.button
                [ HP.classes [ H.ClassName "Panel-toggleButton" ]
                , HE.onClick \_ -> Just Toggle
                ]
                [ HH.text "Close" ]
            ]
        , HH.div
            [ HP.classes [ H.ClassName "Panel-content" ] ]
            [ HH.slot _inner unit innerComponent state.input (Just <<< HandleInner) ]
        ]
  | otherwise =
      HH.div
        [ HP.classes [ H.ClassName "Panel", H.ClassName "Panel--closed" ] ]
        [ HH.div
            [ HP.classes [ H.ClassName "Panel-header" ] ]
            [ HH.button
                [ HP.classes [ H.ClassName "Panel-toggleButton" ]
                , HE.onClick \_ -> Just Toggle
                ]
                [ HH.text "Open" ]
            ]
        ]

handleAction
  :: forall f i o m
   . Action o
  -> H.HalogenM (State i) (Action o) (ChildSlots f o) (Message o) m Unit
handleAction = case _ of
  Toggle -> do
    st' <- H.modify \st -> st { open = not st.open }
    H.raise (if st'.open then Opened else Closed)
  HandleInner msg -> do
    H.raise (Bubble msg)

handleQuery
  :: forall f i o m a
   . Query f a
  -> H.HalogenM (State i) (Action o) (ChildSlots f o) (Message o) m (Maybe a)
handleQuery = case _ of
  SetOpen b a -> do
    H.modify_ (_ { open = b })
    pure (Just a)
  QueryInner q ->
    H.query _inner unit q
