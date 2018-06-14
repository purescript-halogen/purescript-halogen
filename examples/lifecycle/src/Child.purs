module Example.Lifecycle.Child where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH

data Query a
  = Initialize a
  | Finalize a
  | Report String a

data Message
  = Initialized
  | Finalized
  | Reported String

type Slot = Unit

type ChildSlots =
  ( cell :: H.Slot Query Message Int
  )

_cell = SProxy :: SProxy "cell"

child :: Int -> H.Component HH.HTML Query Unit Message Aff
child initialState = H.component
  { initialState: const initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: const Nothing
  }
  where
  render :: Int -> H.ComponentHTML Query ChildSlots Aff
  render id =
    HH.div_
      [ HH.text ("Child " <> show id)
      , HH.ul_
        [ HH.slot _cell 0 (cell 0) unit (listen 0)
        , HH.slot _cell 1 (cell 1) unit (listen 1)
        , HH.slot _cell 2 (cell 2) unit (listen 2)
        ]
      ]

  eval :: Query ~> H.HalogenM Int Query ChildSlots Message Aff
  eval (Initialize next) = do
    id <- H.get
    H.liftEffect $ log ("Initialize Child " <> show id)
    H.raise Initialized
    pure next
  eval (Finalize next) = do
    id <- H.get
    H.liftEffect $ log ("Finalize Child " <> show id)
    H.raise Finalized
    pure next
  eval (Report msg next) = do
    id <- H.get
    H.liftEffect $ log $ "Child " <> show id <> " >>> " <> msg
    H.raise (Reported msg)
    pure next

  listen :: Int -> Message -> Maybe (Query Unit)
  listen i = Just <<< case _ of
    Initialized -> H.action $ Report ("Heard Initialized from cell" <> show i)
    Finalized -> H.action $ Report ("Heard Finalized from cell" <> show i)
    Reported msg -> H.action $ Report ("Re-reporting from cell" <> show i <> ": " <> msg)

cell :: Int -> H.Component HH.HTML Query Unit Message Aff
cell initialState = H.component
  { initialState: const initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: const Nothing
  }
  where

  render :: forall f m. Int -> H.ComponentHTML f () m
  render id =
    HH.li_ [ HH.text ("Cell " <> show id) ]

  eval :: Query ~> H.HalogenM Int Query () Message Aff
  eval (Initialize next) = do
    id <- H.get
    H.liftEffect $ log ("Initialize Cell " <> show id)
    H.raise Initialized
    pure next
  eval (Finalize next) = do
    id <- H.get
    H.liftEffect $ log ("Finalize Cell " <> show id)
    H.raise Finalized
    pure next
  eval (Report msg next) =
    -- A `cell` doesn't have children, so cannot listen and `Report`.
    pure next
