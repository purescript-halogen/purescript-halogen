module Child where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Functor.Coproduct (Coproduct)
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

type StateP eff = H.ParentState Int Int Query Query (ChildEff eff) Int

type QueryP = Coproduct Query (H.ChildF Int Query)

child :: forall eff. H.Component (StateP eff) QueryP (ChildEff eff)
child = H.lifecycleParentComponent
  { render: render
  , eval: eval
  , peek: Nothing
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }
  where

  render :: Int -> H.ParentHTML Int Query Query (ChildEff eff) Int
  render id =
    HH.div [ HP.ref (H.action <<< Ref) ]
      [ HH.text ("Child " <> show id)
      , HH.ul_
        [ HH.slot 0 \_ -> { component: cell, initialState: 0 }
        , HH.slot 1 \_ -> { component: cell, initialState: 1 }
        , HH.slot 2 \_ -> { component: cell, initialState: 2 }
        ]
      ]

  eval :: Query ~> H.ParentDSL Int Int Query Query (ChildEff eff) Int
  eval (Initialize next) = do
    id <- H.get
    H.fromAff $ log ("Initialize Child " <> show id)
    pure next
  eval (Finalize next) = do
    id <- H.get
    H.fromAff $ log ("Finalize Child " <> show id)
    pure next
  eval (Ref _ next) = do
    id <- H.get
    H.fromAff $ log ("Ref Child " <> show id)
    pure next

cell :: forall eff. H.Component Int Query (ChildEff eff)
cell = H.lifecycleComponent
  { render: render
  , eval: eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }
  where

  render :: Int -> H.ComponentHTML Query
  render id =
    HH.li_ [ HH.text ("Cell " <> show id) ]

  eval :: Query ~> H.ComponentDSL Int Query (ChildEff eff)
  eval (Initialize next) = do
    id <- H.get
    H.fromAff $ log ("Initialize Cell " <> show id)
    pure next
  eval (Ref _ next) = do
    id <- H.get
    H.fromAff $ log ("Ref Cell " <> show id)
    pure next
  eval (Finalize next) = do
    id <- H.get
    H.fromAff $ log ("Finalize Cell " <> show id)
    pure next
