module Example.Lifecycle.Child where

import Prelude

import Control.Monad.Aff (Aff())
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE())

import Data.Maybe (Maybe(..))
import Data.Functor.Coproduct (Coproduct())
import Data.Functor.Aff (liftAff)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

data Query a = Initialize a
             | Finalize a

type Slot = Unit

type ChildEff eff = Aff (console :: CONSOLE | eff)

type StateP eff = InstalledState Int Int Query Query (ChildEff eff) Int

type QueryP = Coproduct Query (ChildF Int Query)

child :: forall eff. Component (StateP eff) QueryP (ChildEff eff)
child =
  parentComponentSpec
    { render: render
    , eval: eval
    , initializer: Just (action Initialize)
    , finalizer: Just (action Finalize)
    }

  where

  render :: Int -> ParentHTML Int Query Query (ChildEff eff) Int
  render id =
    H.div_
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
    liftAff $ log ("Initialize Child " <> show id)
    pure next
  eval (Finalize next) = do
    id <- get
    liftAff $ log ("Finalize Child " <> show id)
    pure next

cell :: forall eff. Component Int Query (ChildEff eff)
cell =
  componentSpec
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
    liftAff $ log ("Initialize Cell " <> show id)
    pure next
  eval (Finalize next) = do
    id <- get
    liftAff $ log ("Finalize Cell " <> show id)
    pure next

