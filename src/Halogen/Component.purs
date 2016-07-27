module Halogen.Component where

import Prelude

import Control.Monad.Free (Free, liftF, hoistFree)

import Data.Const (Const)
import Data.Coyoneda (liftCoyoneda)
import Data.Lazy (Lazy, defer)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Halogen.Component.ChildPath (ChildPath, prjQuery, injQuery)
import Halogen.Component.Hook (Hook, Finalized, lmapHook, rmapHook, mapFinalized)
import Halogen.Component.Tree (Tree, mkTree', emptyTree, graftTree)
import Halogen.HTML.Core (HTML)
import Halogen.Query.ChildQuery (ChildQueryF(..), mkChildQuery)
import Halogen.Query.HalogenF (HalogenF(..), hoistHalogenF, hoistHalogenM)

import Unsafe.Coerce (unsafeCoerce)

data ComponentSlot g m p = ComponentSlot p (Lazy (Component g m))

type ParentHTML f p = HTML p (f Unit)
type ParentDSL s f g m p = HalogenF s f g m p

type ComponentHTML f = HTML Void (f Unit)
type ComponentDSL s f m = HalogenF s f (Const Void) m Void

--------------------------------------------------------------------------------

type Component' s f g m p =
  { initialState :: s
  , render :: s -> RenderResult f m
  , eval :: f ~> Free (ParentDSL s f g m p)
  , initializer :: Maybe (f Unit)
  , finalizers :: s -> Array (Finalized m)
  }

type RenderResult f m =
  { hooks :: Array (Hook f m)
  , tree  :: Tree f Unit
  }

emptyResult :: forall f m. RenderResult f m
emptyResult =
  { hooks: []
  , tree: emptyTree
  }

--------------------------------------------------------------------------------

data Component (f :: * -> *) (m :: * -> *)

mkComponent
  :: forall s f g m p
   . Component' s f g m p
  -> Component f m
mkComponent = unsafeCoerce

unComponent
  :: forall f m r
   . (forall s g p. Component' s f g m p -> r)
  -> Component f m
  -> r
unComponent = unsafeCoerce

--------------------------------------------------------------------------------

-- | A spec for a component.
type ComponentSpec s f m =
  { initialState :: s
  , render :: s -> ComponentHTML f
  , eval :: f ~> Free (ComponentDSL s f m)
  }

-- | Builds a self-contained component with no possible children.
component :: forall s f m. ComponentSpec s f m -> Component f m
component spec =
  lifecycleComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , initializer: Nothing
    -- , finalizer: Nothing
    }

-- | A spec for a component, including lifecycle inputs.
type LifecycleComponentSpec s f m =
  { initialState :: s
  , render :: s -> ComponentHTML f
  , eval :: f ~> Free (ComponentDSL s f m)
  , initializer :: Maybe (f Unit)
  -- , finalizer :: Maybe (f Unit)
  }

-- | Builds a self-contained component with lifecycle inputs and no possible
-- | children.
lifecycleComponent
  :: forall s f m
   . LifecycleComponentSpec s f m
  -> Component f m
lifecycleComponent spec =
  mkComponent
    { initialState: spec.initialState
    , render: \s -> { hooks: [], tree: renderTree (spec.render s) }
    , eval: spec.eval
    , initializer: spec.initializer
    , finalizers: \s -> [] -- TODO: maybe [] (\i -> [finalized spec.eval s i]) spec.finalizer
    }
  where
  renderTree :: ComponentHTML f -> Tree f Unit
  renderTree html = mkTree'
    { slot: unit
    , html: defer \_ -> unsafeCoerce html -- CHECK: Safe because p is Void
    , eq: \_ _ -> false -- CHECK: Absurd
    , thunk: false
    }

--------------------------------------------------------------------------------

getSlots
  :: forall s f g m p
   . Free (HalogenF s f g m p) (L.List p)
getSlots = liftF $ GetSlots id

query
  :: forall s f g m p a
   . Applicative m
  => g a
  -> p
  -> Free (HalogenF s f g m p) (Maybe a)
query q p =
  liftF $ RunQuery p $ mkChildQuery $ ChildQueryF $ liftCoyoneda q

queryAll
  :: forall s f g m p a
   . (Applicative m, Ord p)
  => p
  -> g a
  -> Free (HalogenF s f g m p) (M.Map p a)
queryAll p q
  = M.fromList <<< L.catMaybes
  <$> (traverse (\p -> map (Tuple p) <$> query q p) =<< getSlots)

--------------------------------------------------------------------------------

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
  :: forall f f' m
   . Functor m
  => (f ~> f')
  -> (forall a. f' a -> Maybe (f a))
  -> Component f m
  -> Component f' m
transform reviewQ previewQ =
  unComponent \c ->
    mkComponent
      { initialState: c.initialState
      , render: remapRenderResult <<< c.render
      , eval:
          maybe
            (liftF Halt)
            (hoistFree (hoistHalogenF reviewQ) <<< c.eval)
              <<< previewQ
      , initializer: reviewQ <$> c.initializer
      , finalizers: c.finalizers
      }
  where

  remapRenderResult :: RenderResult f m -> RenderResult f' m
  remapRenderResult { hooks, tree } =
    { hooks: lmapHook reviewQ <$> hooks
    , tree: graftTree reviewQ id tree
    }

-- | Transforms a `Component`'s types using a `ChildPath` definition.
transformChild
  :: forall f f' m p p'
   . Functor m
  => ChildPath f f' p p'
  -> Component f m
  -> Component f' m
transformChild i = transform (injQuery i) (prjQuery i)

-- | Changes the component's `m` type. A use case for this would be to interpret
-- | some `Free` monad as `Aff` so the component can be used with `runUI`.
interpret
  :: forall f m m'
   . Functor m'
  => m ~> m'
  -> Component f m
  -> Component f m'
interpret nat =
  unComponent \c ->
    mkComponent
      { initialState: c.initialState
      , render: remapRenderResult <<< c.render
      , eval: hoistFree (hoistHalogenM nat) <<< c.eval
      , initializer: c.initializer
      , finalizers: map (mapFinalized nat) <$> c.finalizers
      }
  where
  remapRenderResult :: RenderResult f m -> RenderResult f m'
  remapRenderResult { hooks, tree } =
    { hooks: rmapHook nat <$> hooks
    , tree
    }
