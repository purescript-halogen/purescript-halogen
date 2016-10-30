module Child where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Lazy (defer)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import DOM.HTML.Types (HTMLElement)

data Query a
  = Initialize a
  | Finalize a
  | Ref (Maybe HTMLElement) a
  | Report String a

data Message
  = Initialized
  | Finalized
  | Refd
  | Reported String

type Slot = Unit

type ChildEff eff = Aff (console :: CONSOLE | eff)

child :: forall eff. Int -> H.Component HH.HTML Query Message (ChildEff eff)
child initialState = H.lifecycleParentComponent
  { render: render
  , eval: eval
  , initialState
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }
  where

  render :: Int -> H.ParentHTML Query Query Int (ChildEff eff)
  render id =
    HH.div [ HP.ref (H.action <<< Ref) ]
      [ HH.text ("Child " <> show id)
      , HH.ul_
        [ HH.slot 0 (defer \_ -> cell 0) (listen 0)
        , HH.slot 1 (defer \_ -> cell 1) (listen 1)
        , HH.slot 2 (defer \_ -> cell 2) (listen 2)
        ]
      ]

  eval :: Query ~> H.ParentDSL Int Query Query Int Message (ChildEff eff)
  eval (Initialize next) = do
    id <- H.get
    H.liftAff $ log ("Initialize Child " <> show id)
    H.raise Initialized
    pure next
  eval (Finalize next) = do
    id <- H.get
    H.liftAff $ log ("Finalize Child " <> show id)
    H.raise Finalized
    pure next
  eval (Ref _ next) = do
    id <- H.get
    H.liftAff $ log ("Ref Child " <> show id)
    H.raise Refd
    pure next
  eval (Report msg next) = do
    H.liftAff $ log $ "Child >>> " <> msg
    H.raise (Reported msg)
    pure next

  listen :: Int -> Message -> Maybe (Query Unit)
  listen i = Just <<< case _ of
    Initialized -> H.action $ Report ("Heard Initialized from cell" <> show i)
    Finalized -> H.action $ Report ("Heard Finalized from cell" <> show i)
    Refd -> H.action $ Report ("Heard Refd from cell" <> show i)
    Reported msg -> H.action $ Report ("Re-reporting from cell" <> show i <> ": " <> msg)

cell :: forall eff. Int -> H.Component HH.HTML Query Message (ChildEff eff)
cell initialState = H.lifecycleComponent
  { render: render
  , eval: eval
  , initialState
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }
  where

  render :: Int -> H.ComponentHTML Query
  render id =
    HH.li_ [ HH.text ("Cell " <> show id) ]

  eval :: Query ~> H.ComponentDSL Int Query Message (ChildEff eff)
  eval (Initialize next) = do
    id <- H.get
    H.liftAff $ log ("Initialize Cell " <> show id)
    H.raise Initialized
    pure next
  eval (Ref _ next) = do
    id <- H.get
    H.liftAff $ log ("Ref Cell " <> show id)
    H.raise Refd
    pure next
  eval (Finalize next) = do
    id <- H.get
    H.liftAff $ log ("Finalize Cell " <> show id)
    H.raise Finalized
    pure next
  eval (Report msg next) = do
    H.liftAff $ log $ "Cell >>> " <> msg
    H.raise (Reported msg)
    pure next
