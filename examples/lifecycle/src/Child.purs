module Child where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Maybe (Maybe(..))

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

type ChildEff eff = Aff (console :: CONSOLE | eff)

child :: forall eff. Int -> H.Component HH.HTML Query Unit Message (ChildEff eff)
child initialState = H.lifecycleParentComponent
  { initialState: const initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: const Nothing
  }
  where

  render :: Int -> H.ParentHTML Query Query Int (ChildEff eff)
  render id =
    HH.div_
      [ HH.text ("Child " <> show id)
      , HH.ul_
        [ HH.slot 0 (cell 0) unit (listen 0)
        , HH.slot 1 (cell 1) unit (listen 1)
        , HH.slot 2 (cell 2) unit (listen 2)
        ]
      ]

  eval :: Query ~> H.ParentDSL Int Query Query Int Message (ChildEff eff)
  eval (Initialize next) = do
    id <- H.get
    H.lift $ do
      -- later' 100 $ pure unit
      log ("Initialize Child " <> show id)
    H.raise Initialized
    pure next
  eval (Finalize next) = do
    id <- H.get
    H.liftAff $ log ("Finalize Child " <> show id)
    H.raise Finalized
    pure next
  eval (Report msg next) = do
    id <- H.get
    H.liftAff $ log $ "Child " <> show id <> " >>> " <> msg
    H.raise (Reported msg)
    pure next

  listen :: Int -> Message -> Maybe (Query Unit)
  listen i = Just <<< case _ of
    Initialized -> H.action $ Report ("Heard Initialized from cell" <> show i)
    Finalized -> H.action $ Report ("Heard Finalized from cell" <> show i)
    Reported msg -> H.action $ Report ("Re-reporting from cell" <> show i <> ": " <> msg)

cell :: forall eff. Int -> H.Component HH.HTML Query Unit Message (ChildEff eff)
cell initialState = H.lifecycleComponent
  { initialState: const initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: const Nothing
  }
  where

  render :: Int -> H.ComponentHTML Query
  render id =
    HH.li_ [ HH.text ("Cell " <> show id) ]

  eval :: Query ~> H.ComponentDSL Int Query Message (ChildEff eff)
  eval (Initialize next) = do
    id <- H.get
    H.lift $ do
      -- later' 150 $ pure unit
      log ("Initialize Cell " <> show id)
    H.raise Initialized
    pure next
  eval (Finalize next) = do
    id <- H.get
    H.liftAff $ log ("Finalize Cell " <> show id)
    H.raise Finalized
    pure next
  eval (Report msg next) =
    -- A `cell` doesn't have children, so cannot listen and `Report`.
    pure next
