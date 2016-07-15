module Halogen.Component where

import Prelude

import Control.Monad.Free (Free)

import Data.Const (Const)
import Data.Lazy (defer)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)

import Halogen.Component.Hook (finalized)
import Halogen.Component.Tree (Tree, mkTree')
import Halogen.Component.Types (Component, mkComponent)
import Halogen.HTML.Core (HTML)
import Halogen.Query.HalogenF (HalogenF)
import Halogen.Query.QueryF (ComponentF, ParentF)

import Unsafe.Coerce (unsafeCoerce)

type ParentHTML f f' g p = HTML p (ParentF f f' g p Unit)
type ParentDSL s f f' g p = Free (HalogenF s (ParentF f f' g p) g)
type ParentState s f' g p = { state :: s, children :: M.Map p (Component f' g) }

type ComponentHTML f g = HTML Void (ComponentF f g Unit)
type ComponentDSL s f g = Free (HalogenF s (ComponentF f g) g)
type ComponentState s f' g p = { state :: s, children :: M.Map Void (Component (Const Void) g) }

-- | A spec for a component.
type ComponentSpec s f g =
  { initialState :: s
  , render :: s -> ComponentHTML f g
  , eval :: ComponentF f g ~> ComponentDSL s f g
  }

-- | Builds a self-contained component with no possible children.
component :: forall s f g. ComponentSpec s f g -> Component (ComponentF f g) g
component spec =
  lifecycleComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , initializer: Nothing
    , finalizer: Nothing
    }

-- | A spec for a component, including lifecycle inputs.
type LifecycleComponentSpec s f g =
  { initialState :: s
  , render :: s -> ComponentHTML f g
  , eval :: ComponentF f g ~> ComponentDSL s f g
  , initializer :: Maybe (ComponentF f g Unit)
  , finalizer :: Maybe (ComponentF f g Unit)
  }

-- | Builds a self-contained component with lifecycle inputs and no possible
-- | children.
lifecycleComponent :: forall s f g. LifecycleComponentSpec s f g -> Component (ComponentF f g) g
lifecycleComponent spec =
  mkComponent
    { state: spec.initialState
    , render: \s -> { state: s, hooks: [], tree: renderTree (spec.render s) }
    , eval: spec.eval
    , initializer: spec.initializer
    , finalizers: \s -> maybe [] (\i -> [finalized spec.eval s i]) spec.finalizer
    }
  where
  renderTree :: ComponentHTML f g -> Tree (ComponentF f g) Unit
  renderTree html = mkTree'
    { slot: unit
    , html: defer \_ -> unsafeCoerce html -- Safe because p is Void
    , eq: \_ _ -> false -- Absurd
    , thunk: false
    }
