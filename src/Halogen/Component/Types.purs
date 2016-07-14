module Halogen.Component.Types
  ( Component
  , Component'
  , RenderResult
  , ComponentHTML
  , ComponentDSL
  , ComponentSpec
  , component
  , mkComponent
  , unComponent
  , LifecycleComponentSpec
  , lifecycleComponent
  , transform
  , transformChild
  , interpret
  )
  where

import Prelude

import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Free.Trans as FT

import Data.Bifunctor (lmap)
import Data.Lazy (defer)
import Data.Maybe (Maybe(..), maybe)

import Halogen.Component.ChildPath (ChildPath, prjQuery, injQuery)
import Halogen.Component.Hook (Hook, Finalized, finalized, mapFinalized, lmapHook, rmapHook)
import Halogen.Component.Tree (Tree, graftTree, mkTree', emptyTree)
import Halogen.HTML.Core (HTML)
import Halogen.Query.EventSource (EventSource(..), runEventSource)
import Halogen.Query.HalogenF (HalogenF(..), hoistHalogenF)
import Halogen.Query.StateF (StateF(..))

import Unsafe.Coerce (unsafeCoerce)

-- | Data type for Halogen components.
-- | - `s` - the component's state
-- | - `f` - the component's query algebra
-- | - `g` - a functor integrated into the component's query algebra that allows
-- |         embedding of external DSLs or handling of effects.
type Component' s f g =
  { state :: s
  , render :: s -> RenderResult s f g
  , eval :: f ~> ComponentDSL s f g
  , initializer :: Maybe (f Unit)
  , finalizers :: s -> Array (Finalized g)
  }

type RenderResult s f g =
  { state :: s
  , hooks :: Array (Hook f g)
  , tree  :: Tree f Unit
  }

emptyResult :: forall s f g. s -> RenderResult s f g
emptyResult state =
  { state
  , hooks: []
  , tree: emptyTree
  }

data Component (f :: * -> *) (g :: * -> *)

mkComponent :: forall s f g. Component' s f g -> Component f g
mkComponent = unsafeCoerce

unComponent :: forall f g r. (forall s. Component' s f g -> r) -> Component f g -> r
unComponent f = f <<< unsafeCoerce

-- | The type for `HTML` rendered by a self-contained component.
type ComponentHTML p f = HTML p (f Unit)

-- | The DSL used in the `eval` function for self-contained components.
type ComponentDSL s f g = Free (HalogenF s f g)

-- | A spec for a component.
type ComponentSpec s f g p =
  { initialState :: s
  , render :: s -> ComponentHTML p f
  , eval :: f ~> ComponentDSL s f g
  }

-- | Builds a self-contained component with no possible children.
component :: forall s f g p. ComponentSpec s f g p -> Component' s f g
component spec =
  lifecycleComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , initializer: Nothing
    , finalizer: Nothing
    }

-- | A spec for a component, including lifecycle inputs.
type LifecycleComponentSpec s f g p =
  { initialState :: s
  , render :: s -> ComponentHTML p f
  , eval :: f ~> ComponentDSL s f g
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

-- | Builds a self-contained component with lifecycle inputs and no possible
-- | children.
lifecycleComponent :: forall s f g p. LifecycleComponentSpec s f g p -> Component' s f g
lifecycleComponent spec =
  { state: spec.initialState
  , render: \s -> { state: s, hooks: [], tree: renderTree (spec.render s) }
  , eval: spec.eval
  , initializer: spec.initializer
  , finalizers: \s -> maybe [] (\i -> [finalized spec.eval s i]) spec.finalizer
  }
  where
  renderTree :: ComponentHTML p f -> Tree f Unit
  renderTree html = mkTree'
    { slot: unit
    , html: defer \_ -> unsafeCoerce html -- Safe because p is Void
    , eq: \_ _ -> false -- Absurd
    , thunk: false
    }

-- | Transforms a `Component`'s types using partial mapping functions.
-- |
-- | If the initial state provided to the component fails the transformation an
-- | empty component will be rendered. If either of the transformations fail the
-- | component will "halt" (evaluate to `empty`), so care must be taken when
-- | handling transformed components to ensure they receive the intended query
-- | values and initial state type.
-- |
-- | Halogen itself will never cause a `transform`ed component to halt; this
-- | situation will only arise when the initial state is incorrect or a bad
-- | externally constructed query is passed to the component.
transform
  :: forall f f' g
   . Functor g
  => (f ~> f')
  -> (forall a. f' a -> Maybe (f a))
  -> Component f g
  -> Component f' g
transform reviewQ previewQ =
  unComponent \c ->
    mkComponent
      { state: c.state
      , render: remapRenderResult <<< c.render
      , eval: maybe (liftF HaltHF) (hoistFree remapF <<< c.eval) <<< previewQ
      , initializer: reviewQ <$> c.initializer
      , finalizers: c.finalizers
      }
  where

  remapRenderResult :: forall s. RenderResult s f g -> RenderResult s f' g
  remapRenderResult { state, hooks, tree } =
    { state
    , hooks: lmapHook reviewQ <$> hooks
    , tree: graftTree reviewQ id tree
    }

  remapF :: forall s. HalogenF s f g ~> HalogenF s f' g
  remapF = case _ of
    StateHF (Get k) -> StateHF (Get k)
    StateHF (Modify f next) -> StateHF (Modify f next)
    SubscribeHF es next -> SubscribeHF (EventSource (FT.interpret (lmap reviewQ) (runEventSource es))) next
    QueryFHF q -> QueryFHF (reviewQ q)
    QueryGHF q -> QueryGHF q
    RenderHF p a -> RenderHF p a
    RenderPendingHF k -> RenderPendingHF k
    HaltHF ->  HaltHF

-- | Transforms a `Component`'s types using a `ChildPath` definition.
transformChild
  :: forall f f' g p p'
   . Functor g
  => ChildPath f f' p p'
  -> Component f g
  -> Component f' g
transformChild i = transform (injQuery i) (prjQuery i)

-- | Changes the component's `g` type. A use case for this would be to interpret
-- | some `Free` monad as `Aff` so the component can be used with `runUI`.
interpret
  :: forall f g g'
   . Functor g'
  => g ~> g'
  -> Component f g
  -> Component f g'
interpret nat =
  unComponent \c ->
    mkComponent
      { state: c.state
      , render: remapRenderResult <<< c.render
      , eval: hoistFree (hoistHalogenF nat) <<< c.eval
      , initializer: c.initializer
      , finalizers: map (mapFinalized nat) <$> c.finalizers
      }
  where
  remapRenderResult :: forall s. RenderResult s f g -> RenderResult s f g'
  remapRenderResult { state, hooks, tree } =
    { state
    , hooks: rmapHook nat <$> hooks
    , tree
    }
