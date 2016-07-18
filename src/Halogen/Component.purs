module Halogen.Component where

import Prelude

import Control.Monad.Free (Free, liftF)

import Data.Const (Const)
import Data.Functor.Coproduct (Coproduct, left, right)
import Data.Lazy (defer)
import Data.List (List)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

import Halogen.Component.Hook (Hook, Finalized)
import Halogen.Component.Tree (Tree, mkTree', emptyTree)
import Halogen.HTML.Core (HTML)
import Halogen.Query.HalogenF (HalogenF(..))

import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------

data QueryF f g p a
  = GetChildren (M.Map p (Component f g) -> a)
  | RunQuery ((Component f g -> f ~> g) -> g a)

instance functorQueryF :: Functor g => Functor (QueryF f g p) where
  map f = case _ of
    GetChildren k -> GetChildren (map f k)
    RunQuery k -> RunQuery (map f <<< k)

--------------------------------------------------------------------------------

type ParentF f f' g p = Coproduct (QueryF f' g p) (Coproduct f f')
type ComponentF f g = ParentF f (Const Void) g Void

type ParentHTML f f' g p = HTML p (ParentF f f' g p Unit)
type ParentDSL s f f' g p = HalogenF s (ParentF f f' g p) g
type ParentState s f' g p = { state :: s, children :: M.Map p (Component f' g) }

type ComponentHTML f g = HTML Void (ComponentF f g Void)
type ComponentDSL s f g = HalogenF s (ComponentF f g) g
type ComponentState s f' g p = { state :: s, children :: M.Map Void (Component (Const Void) g) }

--------------------------------------------------------------------------------

-- | Data type for Halogen components.
-- | - `s` - the component's state
-- | - `f` - the component's query algebra
-- | - `g` - a functor integrated into the component's query algebra that allows
-- |         embedding of external DSLs or handling of effects.
type Component' s f f' g p =
  { state :: ParentState s f' g p
  , render :: s -> RenderResult s f f' g p
  , eval :: f ~> Free (ParentDSL s f f' g p)
  , initializer :: Maybe (ParentF f f' g p Unit)
  , finalizers :: s -> Array (Finalized g)
  }

type RenderResult s f f' g p =
  { state :: s
  , hooks :: Array (Hook f g)
  , tree  :: Tree (ParentF f f' g p) Unit
  }

emptyResult :: forall s f f' g p. s -> RenderResult s f f' g p
emptyResult state =
  { state
  , hooks: []
  , tree: emptyTree
  }

--------------------------------------------------------------------------------

data Component (f :: * -> *) (g :: * -> *)

mkComponent
  :: forall s f f' g p
   . Component' s f f' g p
  -> Component f g
mkComponent = unsafeCoerce

unComponent
  :: forall f g r
   . (forall s f' p. Component' s f f' g p -> r)
  -> Component f g
  -> r
unComponent f = f <<< unsafeCoerce

-- --------------------------------------------------------------------------------
--
-- -- | A spec for a component.
-- type ComponentSpec s f g =
--   { initialState :: s
--   , render :: s -> ComponentHTML f g
--   , eval :: ComponentF f g ~> ComponentDSL s f g
--   }
--
-- -- | Builds a self-contained component with no possible children.
-- component :: forall s f g. ComponentSpec s f g -> Component (ComponentF f g) g
-- component spec =
--   lifecycleComponent
--     { initialState: spec.initialState
--     , render: spec.render
--     , eval: spec.eval
--     , initializer: Nothing
--     , finalizer: Nothing
--     }

-- | A spec for a component, including lifecycle inputs.
type LifecycleComponentSpec s f g =
  { initialState :: s
  , render :: s -> ComponentHTML f g
  , eval :: f ~> Free (ComponentDSL s f g)
  , initializer :: Maybe (f Unit)
  -- , finalizer :: Maybe (f Unit)
  }

-- | Builds a self-contained component with lifecycle inputs and no possible
-- | children.
lifecycleComponent
  :: forall s f g
   . LifecycleComponentSpec s f g
  -> Component f g
lifecycleComponent spec =
  mkComponent
    { state: { state: spec.initialState, children: M.empty }
    , render: \s -> { state: s, hooks: [], tree: renderTree (spec.render s) }
    , eval: spec.eval
    , initializer:  right <<< left <$> spec.initializer
    , finalizers: \s -> [] -- TODO: maybe [] (\i -> [finalized spec.eval s i]) spec.finalizer
    }
  where
  renderTree :: ComponentHTML f g -> Tree (ComponentF f g) Unit
  renderTree html = mkTree'
    { slot: unit
    , html: defer \_ -> unsafeCoerce html -- CHECK: Safe because p is Void
    , eq: \_ _ -> false -- CHECK: Absurd
    , thunk: false
    }

--------------------------------------------------------------------------------

getChildren
  :: forall s f f' g p
   . Free (HalogenF s (ParentF f f' g p) g) (M.Map p (Component f' g))
getChildren = liftF $ QueryFHF $ left $ GetChildren id

getChild
  :: forall s f f' g p
   . Ord p
  => p
  -> Free (HalogenF s (ParentF f f' g p) g) (Maybe (Component f' g))
getChild p = M.lookup p <$> getChildren

runQuery
  :: forall s f f' g p a
   . Applicative g
  => f' a
  -> Component f' g
  -> Free (HalogenF s (ParentF f f' g p) g) a
runQuery q c = do
  x <- liftF $ QueryFHF $ left $ RunQuery \k -> k c q
  liftF $ QueryGHF (pure x)

query
  :: forall s f f' g p a
   . (Applicative g, Ord p)
  => p
  -> f' a
  -> Free (HalogenF s (ParentF f f' g p) g) (Maybe a)
query p q =
  getChild p >>= case _ of
    Nothing -> pure Nothing
    Just comp -> Just <$> runQuery q comp

queryAll
  :: forall s f f' g p a
   . (Applicative g, Ord p)
  => p
  -> f' a
  -> Free (HalogenF s (ParentF f f' g p) g) (M.Map p a)
queryAll p q = traverse (runQuery q) =<< getChildren

childSlots
  :: forall s f f' g p a
   . (Applicative g, Ord p)
  => p
  -> f' a
  -> Free (HalogenF s (ParentF f f' g p) g) (List p)
childSlots p q = M.keys <$> getChildren

--------------------------------------------------------------------------------

-- -- | Transforms a `Component`'s types using partial mapping functions.
-- -- |
-- -- | If the initial state provided to the component fails the transformation an
-- -- | empty component will be rendered. If either of the transformations fail the
-- -- | component will "halt" (evaluate to `empty`), so care must be taken when
-- -- | handling transformed components to ensure they receive the intended query
-- -- | values and initial state type.
-- -- |
-- -- | Halogen itself will never cause a `transform`ed component to halt; this
-- -- | situation will only arise when the initial state is incorrect or a bad
-- -- | externally constructed query is passed to the component.
-- transform
--   :: forall f f' g
--    . Functor g
--   => (f ~> f')
--   -> (forall a. f' a -> Maybe (f a))
--   -> Component f g
--   -> Component f' g
-- transform reviewQ previewQ =
--   unComponent \c ->
--     mkComponent
--       { state: c.state
--       , render: remapRenderResult <<< c.render
--       , eval: maybe (liftF HaltHF) (hoistFree remapF <<< c.eval) <<< previewQ
--       , initializer: reviewQ <$> c.initializer
--       , finalizers: c.finalizers
--       }
--   where
--
--   remapRenderResult :: forall s. RenderResult s f g -> RenderResult s f' g
--   remapRenderResult { state, hooks, tree } =
--     { state
--     , hooks: lmapHook reviewQ <$> hooks
--     , tree: graftTree reviewQ id tree
--     }
--
--   remapF :: forall s. HalogenF s f g ~> HalogenF s f' g
--   remapF = case _ of
--     StateHF (Get k) -> StateHF (Get k)
--     StateHF (Modify f next) -> StateHF (Modify f next)
--     SubscribeHF es next -> SubscribeHF (EventSource (FT.interpret (lmap reviewQ) (runEventSource es))) next
--     QueryFHF q -> QueryFHF (reviewQ q)
--     QueryGHF q -> QueryGHF q
--     RenderHF p a -> RenderHF p a
--     RenderPendingHF k -> RenderPendingHF k
--     HaltHF ->  HaltHF
--
-- -- | Transforms a `Component`'s types using a `ChildPath` definition.
-- transformChild
--   :: forall f f' g p p'
--    . Functor g
--   => ChildPath f f' p p'
--   -> Component f g
--   -> Component f' g
-- transformChild i = transform (injQuery i) (prjQuery i)
--
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
