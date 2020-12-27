module Example.Lifecycle.Child where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Type.Proxy (Proxy(..))

data Action
  = Initialize
  | Finalize
  | Report String

data Message
  = Initialized
  | Finalized
  | Reported String

type Slot = Unit

type ChildSlots =
  ( cell :: H.Slot (Const Void) Message Int
  )

_cell = Proxy :: Proxy "cell"

child :: forall f. Int -> H.Component f Unit Message Aff
child initialState =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval (H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , finalize = Just Finalize
        })
    }
  where
  render :: Int -> H.ComponentHTML Action ChildSlots Aff
  render id =
    HH.div_
      [ HH.text ("Child " <> show id)
      , HH.ul_
          [ HH.slot _cell 0 (cell 0) unit (listen 0)
          , HH.slot _cell 1 (cell 1) unit (listen 1)
          , HH.slot _cell 2 (cell 2) unit (listen 2)
          ]
      ]

  handleAction :: Action -> H.HalogenM Int Action ChildSlots Message Aff Unit
  handleAction Initialize = do
    id <- H.get
    H.liftEffect $ log ("Initialize Child " <> show id)
    H.raise Initialized
  handleAction Finalize = do
    id <- H.get
    H.liftEffect $ log ("Finalize Child " <> show id)
    H.raise Finalized
  handleAction (Report msg) = do
    id <- H.get
    H.liftEffect $ log $ "Child " <> show id <> " >>> " <> msg
    H.raise (Reported msg)

  listen :: Int -> Message -> Action
  listen i = case _ of
    Initialized -> Report ("Heard Initialized from cell" <> show i)
    Finalized -> Report ("Heard Finalized from cell" <> show i)
    Reported msg -> Report ("Re-reporting from cell" <> show i <> ": " <> msg)

cell :: forall f. Int -> H.Component f Unit Message Aff
cell initialState =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval (H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , finalize = Just Finalize
        })
    }
  where

  render :: forall act m. Int -> H.ComponentHTML act () m
  render id =
    HH.li_ [ HH.text ("Cell " <> show id) ]

  handleAction :: Action -> H.HalogenM Int Action () Message Aff Unit
  handleAction Initialize = do
    id <- H.get
    H.liftEffect $ log ("Initialize Cell " <> show id)
    H.raise Initialized
  handleAction Finalize = do
    id <- H.get
    H.liftEffect $ log ("Finalize Cell " <> show id)
    H.raise Finalized
  handleAction (Report msg) =
    -- A `cell` doesn't have children, so cannot listen and `Report`.
    pure unit
