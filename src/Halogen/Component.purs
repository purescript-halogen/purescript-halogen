module Halogen.Component where

import Prelude

import Control.Monad.Free (Free, liftF, hoistFree)
import Control.Monad.Free.Trans as FT

import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Functor.Coproduct (Coproduct, coproduct, left, right)
import Data.Lazy (Lazy, defer)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Halogen.Component.Hook (Hook, Finalized, lmapHook)
import Halogen.Component.Tree (Tree, mkTree', emptyTree, graftTree)
import Halogen.HTML.Core (HTML)
import Halogen.Query.HalogenF (HalogenF(..))
import Halogen.Query.EventSource (EventSource(..), runEventSource)
import Halogen.Component.ChildPath (ChildPath, prjQuery, injQuery)

import Unsafe.Coerce (unsafeCoerce)

data ComponentSlot g m p = ComponentSlot p (Lazy (Component g m))

--------------------------------------------------------------------------------

data QueryF f m p a
  = GetSlots (L.List p -> a)
  | RunQuery p (Maybe (f ~> m) -> m a)

instance functorQueryF :: Functor m => Functor (QueryF f m p) where
  map f = case _ of
    GetSlots k -> GetSlots (map f k)
    RunQuery p k -> RunQuery p (map f <<< k)

--------------------------------------------------------------------------------

type ParentF f g m p = Coproduct (QueryF g m p) f
type ComponentF f m = ParentF f (Const Void) m Void

type ParentHTML f g m p = HTML p (ParentF f g m p Unit)
type ParentDSL s f g m p = HalogenF s (ParentF f g m p) m

type ComponentHTML f m = HTML Void (ComponentF f m Void)
type ComponentDSL s f m = HalogenF s (ComponentF f m) m

--------------------------------------------------------------------------------

type Component' s f g m p =
  { initialState :: s
  , render :: s -> RenderResult s f g m p
  , eval :: f ~> Free (ParentDSL s f g m p)
  , initializer :: Maybe (ParentF f g m p Unit)
  , finalizers :: s -> Array (Finalized m)
  }

type RenderResult s f g m p =
  { state :: s
  , hooks :: Array (Hook f m)
  , tree  :: Tree (ParentF f g m p) Unit
  }

emptyResult :: forall s f g m p. s -> RenderResult s f g m p
emptyResult state =
  { state
  , hooks: []
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
unComponent f = f <<< unsafeCoerce

-- --------------------------------------------------------------------------------

-- | A spec for a component.
type ComponentSpec s f m =
  { initialState :: s
  , render :: s -> ComponentHTML f m
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
  , render :: s -> ComponentHTML f m
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
    , render: \s -> { state: s, hooks: [], tree: renderTree (spec.render s) }
    , eval: spec.eval
    , initializer:  right <$> spec.initializer
    , finalizers: \s -> [] -- TODO: maybe [] (\i -> [finalized spec.eval s i]) spec.finalizer
    }
  where
  renderTree :: ComponentHTML f m -> Tree (ComponentF f m) Unit
  renderTree html = mkTree'
    { slot: unit
    , html: defer \_ -> unsafeCoerce html -- CHECK: Safe because p is Void
    , eq: \_ _ -> false -- CHECK: Absurd
    , thunk: false
    }

--------------------------------------------------------------------------------

getSlots
  :: forall s f g m p
   . Free (HalogenF s (ParentF f g m p) m) (L.List p)
getSlots = liftF $ QueryFHF $ left $ GetSlots id

query
  :: forall s f g m p a
   . Applicative m
  => g a
  -> p
  -> Free (HalogenF s (ParentF f g m p) m) (Maybe a)
query q p = do
  liftF $ QueryFHF $ left $ RunQuery p \k ->
    case k of
      Nothing -> pure Nothing
      Just f -> Just <$> f q

queryAll
  :: forall s f g m p a
   . (Applicative m, Ord p)
  => p
  -> g a
  -> Free (HalogenF s (ParentF f g m p) m) (M.Map p a)
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
      , eval: maybe (liftF HaltHF) (hoistFree remapF <<< c.eval) <<< previewQ
      , initializer: remapQ <$> c.initializer
      , finalizers: c.finalizers
      }
  where

  remapRenderResult :: forall s g p. RenderResult s f g m p -> RenderResult s f' g m p
  remapRenderResult { state, hooks, tree } =
    { state
    , hooks: lmapHook reviewQ <$> hooks
    , tree: graftTree remapQ id tree
    }

  remapF :: forall s g p. HalogenF s (ParentF f g m p) m ~> HalogenF s (ParentF f' g m p) m
  remapF = case _ of
    StateHF shf -> StateHF shf
    SubscribeHF es next ->
      let es' = EventSource (FT.interpret (lmap remapQ) (runEventSource es))
      in SubscribeHF es' next
    QueryFHF q -> QueryFHF (remapQ q)
    QueryGHF q -> QueryGHF q
    RenderHF p a -> RenderHF p a
    RenderPendingHF k -> RenderPendingHF k
    HaltHF -> HaltHF

  remapQ :: forall g p. ParentF f g m p ~> ParentF f' g m p
  remapQ = coproduct left (right <<< reviewQ)

-- | Transforms a `Component`'s types using a `ChildPath` definition.
transformChild
  :: forall f f' g p p'
   . Functor g
  => ChildPath f f' p p'
  -> Component f g
  -> Component f' g
transformChild i = transform (injQuery i) (prjQuery i)

-- -- | Changes the component's `g` type. A use case for this would be to interpret
-- -- | some `Free` monad as `Aff` so the component can be used with `runUI`.
-- interpret
--   :: forall f g g'
--    . Functor g'
--   => g ~> g'
--   -> Component f g
--   -> Component f g'
-- interpret nat =
--   unComponent \c ->
--     mkComponent
--       { state: c.state
--       , render: remapRenderResult <<< c.render
--       , eval: hoistFree (hoistHalogenF nat) <<< c.eval
--       , initializer: c.initializer
--       , finalizers: map (mapFinalized nat) <$> c.finalizers
--       }
--   where
--   remapRenderResult :: forall s. RenderResult s f g -> RenderResult s f g'
--   remapRenderResult { state, hooks, tree } =
--     { state
--     , hooks: rmapHook nat <$> hooks
--     , tree
--     }
