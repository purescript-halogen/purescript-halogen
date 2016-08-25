module Child where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Lazy (defer)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP

import DOM.HTML.Types (HTMLElement)

data Query a
  = Initialize a
  | Finalize a
  | Ref (Maybe HTMLElement) a

type Slot = Unit

type ChildEff eff = Aff (console :: CONSOLE | eff)

child :: forall eff. Int -> H.Component Query (ChildEff eff)
child initialState = H.lifecycleParentComponent
  { render: render
  , eval: eval
  , initialState
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }
  where

  render :: Int -> H.ParentHTML Query Query (ChildEff eff) Int
  render id =
    HH.div [ HP.ref (H.action <<< Ref) ]
      [ HH.text ("Child " <> show id)
      , HH.ul_
        [ HH.slot 0 (defer \_ -> cell 0)
        , HH.slot 1 (defer \_ -> cell 1)
        , HH.slot 2 (defer \_ -> cell 2)
        ]
      ]

  eval :: Query ~> H.ParentDSL Int Query Query (ChildEff eff) Int
  eval (Initialize next) = do
    id <- H.get
    H.liftAff $ log ("Initialize Child " <> show id)
    pure next
  eval (Finalize next) = do
    id <- H.get
    H.liftAff $ log ("Finalize Child " <> show id)
    pure next
  eval (Ref _ next) = do
    id <- H.get
    H.liftAff $ log ("Ref Child " <> show id)
    pure next

cell :: forall eff. Int -> H.Component Query (ChildEff eff)
cell initialState = H.lifecycleComponent
  { render: render
  , eval: eval
  , initialState
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }
  where

  render :: Int -> H.ComponentHTML Query (ChildEff eff)
  render id =
    HH.li_ [ HH.text ("Cell " <> show id) ]

  eval :: Query ~> H.ComponentDSL Int Query (ChildEff eff)
  eval (Initialize next) = do
    id <- H.get
    H.liftAff $ log ("Initialize Cell " <> show id)
    pure next
  eval (Ref _ next) = do
    id <- H.get
    H.liftAff $ log ("Ref Cell " <> show id)
    pure next
  eval (Finalize next) = do
    id <- H.get
    H.liftAff $ log ("Finalize Cell " <> show id)
    pure next
