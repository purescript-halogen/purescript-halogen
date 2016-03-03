module Child where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE())

import Data.Maybe (Maybe(..))
import Data.Functor.Coproduct (Coproduct())

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

import DOM.HTML.Types (HTMLElement())

data Query a = Initialize a
             | Finalize a
             | Ref HTMLElement a

type Slot = Unit

type ChildEff eff = Aff (console :: CONSOLE | eff)

type StateP eff = ParentState Int Int Query Query (ChildEff eff) Int

type QueryP = Coproduct Query (ChildF Int Query)

child :: forall eff. Component (StateP eff) QueryP (ChildEff eff)
child = lifecycleParentComponent
  { render: render
  , eval: eval
  , peek: Nothing
  , initializer: Just (action Initialize)
  , finalizer: Just (action Finalize)
  }
  where

  render :: Int -> ParentHTML Int Query Query (ChildEff eff) Int
  render id =
    H.div [ P.ref (action <<< Ref) ]
      [ H.text ("Child " <> show id)
      , H.ul_
        [ H.slot 0 \_ -> { component: cell, initialState: 0 }
        , H.slot 1 \_ -> { component: cell, initialState: 1 }
        , H.slot 2 \_ -> { component: cell, initialState: 2 }
        ]
      ]

  eval :: Natural Query (ParentDSL Int Int Query Query (ChildEff eff) Int)
  eval (Initialize next) = do
    id <- get
    fromAff $ log ("Initialize Child " <> show id)
    pure next
  eval (Finalize next) = do
    id <- get
    fromAff $ log ("Finalize Child " <> show id)
    pure next
  eval (Ref el next) = do
    id <- get
    fromAff $ log ("Ref Child " <> show id)
    pure next

cell :: forall eff. Component Int Query (ChildEff eff)
cell = lifecycleComponent
  { render: render
  , eval: eval
  , initializer: Just (action Initialize)
  , finalizer: Just (action Finalize)
  }
  where

  render :: Int -> ComponentHTML Query
  render id =
    H.li_ [ H.text ("Cell " <> show id) ]

  eval :: Natural Query (ComponentDSL Int Query (ChildEff eff))
  eval (Initialize next) = do
    id <- get
    fromAff $ log ("Initialize Cell " <> show id)
    pure next
  eval (Ref el next) = do
    id <- get
    fromAff $ log ("Ref Cell " <> show id)
    pure next
  eval (Finalize next) = do
    id <- get
    fromAff $ log ("Finalize Cell " <> show id)
    pure next

