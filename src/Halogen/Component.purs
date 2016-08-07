module Halogen.Component
  ( Component
  , component
  , mkComponent
  , unComponent
  , Component'
  , ComponentDSL
  , ComponentHTML
  , ComponentSpec
  , LifecycleComponentSpec
  , ParentDSL
  , ParentHTML
  , RenderResult
  , emptyResult
  , ComponentSlot(..)
  , lifecycleComponent
  , getSlots
  , query
  , queryAll
  , transform
  , transformChild
  , interpret
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Free (Free, liftF, hoistFree)

import Data.Const (Const)
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
import Halogen.Query.ChildQuery (childQuery)
import Halogen.Query.HalogenF (HalogenF(..), hoistHalogenF, hoistHalogenM)
import Halogen.Data.OrdBox (OrdBox)

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
  , mkOrdBox :: p -> OrdBox p
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
    , mkOrdBox: absurd
    }
  where
  renderTree :: ComponentHTML f -> Tree f Unit
  renderTree html = mkTree'
    { slot: unit
    , html: defer \_ -> unsafeCoerce html -- Safe because p is Void
    , eq: \_ _ -> false -- Absurd
    , thunk: false
    }

--------------------------------------------------------------------------------

-- TODO: hide export
mkQuery
  :: forall s f g m p a
   . (Applicative m, Eq p)
  => p
  -> g a
  -> Free (HalogenF s f g m p) a
mkQuery p q = liftF $ ChildQuery (childQuery p q)

getSlots :: forall s f g m p. Free (HalogenF s f g m p) (L.List p)
getSlots = liftF $ GetSlots id

query
  :: forall s f g m p a
   . (Applicative m, Eq p)
  => p
  -> g a
  -> Free (HalogenF s f g m p) (Maybe a)
query p q = do
  slots <- getSlots
  case L.elemIndex p slots of
    Nothing -> pure Nothing
    Just _ -> Just <$> mkQuery p q

queryAll
  :: forall s f g m p a
   . (Applicative m, Ord p)
  => g a
  -> Free (HalogenF s f g m p) (M.Map p a)
queryAll q =
  M.fromList <$> (traverse (\p -> map (Tuple p) (mkQuery p q)) =<< getSlots)

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
      , mkOrdBox: c.mkOrdBox
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
      , render: hoistRenderResult <<< c.render
      , eval: hoistFree (hoistHalogenM nat) <<< c.eval
      , initializer: c.initializer
      , finalizers: map (mapFinalized nat) <$> c.finalizers
      , mkOrdBox: c.mkOrdBox
      }
  where
  hoistRenderResult :: RenderResult f m -> RenderResult f m'
  hoistRenderResult { hooks, tree } =
    { hooks: rmapHook nat <$> hooks
    , tree
    }
