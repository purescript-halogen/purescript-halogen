module Halogen.Component
  ( Component
  , Component'
  , mkComponent
  , unComponent
  , ComponentDSL
  , ParentDSL
  , ComponentHTML
  , ParentHTML
  , ComponentSlot
  , mkComponentSlot
  , unComponentSlot
  , ComponentSpec
  , component
  , LifecycleComponentSpec
  , lifecycleComponent
  , ParentComponentSpec
  , parentComponent
  , ParentLifecycleComponentSpec
  , lifecycleParentComponent
  , getSlots
  , query
  , query'
  , queryAll
  -- TODO: , queryAll'
  , transform
  , transformChild
  , interpret
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Free (liftF)

import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Lazy (Lazy)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Halogen.Component.ChildPath (ChildPath, injSlot, prjQuery, injQuery)
import Halogen.Data.OrdBox (OrdBox, mkOrdBox)
import Halogen.HTML.Core (HTML)
import Halogen.Query.ChildQuery (childQuery)
import Halogen.Query.HalogenF (HalogenF(..))
import Halogen.Query.HalogenM (HalogenM(..))
import Halogen.Query.HalogenM as HM

import Unsafe.Coerce (unsafeCoerce)

data ComponentSlot' g m p i o = ComponentSlot p (Lazy (Component g m o)) (o -> i)

data ComponentSlot (g :: * -> *) (m :: * -> *) p i

instance functorSlotF :: Functor (ComponentSlot g m p) where
  map f = unComponentSlot \p ctor k -> mkComponentSlot p ctor (f <<< k)

mkComponentSlot
  :: forall g m p i o
   . p
  -> (Lazy (Component g m o))
  -> (o -> i)
  -> ComponentSlot g m p i
mkComponentSlot = unsafeCoerce ComponentSlot

unComponentSlot
  :: forall g m p i r
   . (forall o. p -> Lazy (Component g m o) -> (o -> i) -> r)
  -> ComponentSlot g m p i
  -> r
unComponentSlot f cs =
  case unsafeCoerce cs of
    ComponentSlot p ctor k -> f p ctor k

hoistSlotM
  :: forall g m m' p i
   . Functor m'
  => (m ~> m')
  -> ComponentSlot g m p i
  -> ComponentSlot g m' p i
hoistSlotM nat = unComponentSlot \p ctor k ->
  mkComponentSlot p (map (interpret nat) ctor) k

--------------------------------------------------------------------------------

type ParentHTML f g p m = HTML (ComponentSlot g m p (f Unit)) (f Unit)
type ParentDSL = HalogenM

type ComponentHTML f m = ParentHTML f (Const Void) Void m
type ComponentDSL s f m o = ParentDSL s f (Const Void) Void o m

--------------------------------------------------------------------------------

type Component' s f g p o m =
  { initialState :: s
  , render :: s -> ParentHTML f g p m
  , eval :: f ~> ParentDSL s f g p o m
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  , mkOrdBox :: p -> OrdBox p
  }

--------------------------------------------------------------------------------

data Component (f :: * -> *) (m :: * -> *) o

mkComponent
  :: forall s f g p o m
   . Component' s f g p o m
  -> Component f m o
mkComponent = unsafeCoerce

unComponent
  :: forall f m o r
   . (forall s g p. Component' s f g p o m -> r)
  -> Component f m o
  -> r
unComponent = unsafeCoerce

--------------------------------------------------------------------------------

-- | A spec for a component.
type ComponentSpec s f m o =
  { initialState :: s
  , render :: s -> ComponentHTML f m
  , eval :: f ~> ComponentDSL s f m o
  }

-- | Builds a self-contained component with no possible children.
component :: forall s f m o. ComponentSpec s f m o -> Component f m o
component spec =
  lifecycleComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , initializer: Nothing
    , finalizer: Nothing
    }

-- | A spec for a component, including lifecycle inputs.
type LifecycleComponentSpec s f m o i =
  { initialState :: s
  , render :: s -> ComponentHTML f m
  , eval :: f ~> ComponentDSL s f m o
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

-- | Builds a self-contained component with lifecycle inputs and no possible
-- | children.
lifecycleComponent
  :: forall s f m o i
   . LifecycleComponentSpec s f m o i
  -> Component f m o
lifecycleComponent spec =
  mkComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , initializer: spec.initializer
    , finalizer: spec.finalizer
    , mkOrdBox: absurd
    }

type ParentComponentSpec s f g p o m i =
  { initialState :: s
  , render :: s -> ParentHTML f g p m
  , eval :: f ~> ParentDSL s f g p o m
  }

parentComponent
  :: forall s f g p o m i
   . Ord p
  => ParentComponentSpec s f g p o m i
  -> Component f m o
parentComponent spec =
  mkComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , initializer: Nothing
    , finalizer: Nothing
    , mkOrdBox: mkOrdBox
    }

type ParentLifecycleComponentSpec s f g p o m =
  { initialState :: s
  , render :: s -> ParentHTML f g p m
  , eval :: f ~> ParentDSL s f g p o m
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

lifecycleParentComponent
  :: forall s f g p o m
   . Ord p
  => ParentLifecycleComponentSpec s f g p o m
  -> Component f m o
lifecycleParentComponent spec =
  mkComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , initializer: spec.initializer
    , finalizer: spec.finalizer
    , mkOrdBox: mkOrdBox
    }

--------------------------------------------------------------------------------

mkQuery
  :: forall s f g p o m a
   . (Applicative m, Eq p)
  => p
  -> g a
  -> HalogenM s f g p o m a
mkQuery p q = HalogenM $ liftF $ ChildQuery (childQuery p q)

getSlots :: forall s f g p o m. HalogenM s f g p o m (L.List p)
getSlots = HalogenM $ liftF $ GetSlots id

query
  :: forall s f g p o m a
   . (Applicative m, Eq p)
  => p
  -> g a
  -> HalogenM s f g p o m (Maybe a)
query p q = do
  slots <- getSlots
  case L.elemIndex p slots of
    Nothing -> pure Nothing
    Just _ -> Just <$> mkQuery p q

query'
  :: forall s f g g' m p p' o a
   . (Applicative m, Eq p')
  => ChildPath g g' p p'
  -> p
  -> g a
  -> HalogenM s f g' p' o m (Maybe a)
query' i p q = query (injSlot i p) (injQuery i q)

queryAll
  :: forall s f g p o m a
   . (Applicative m, Ord p)
  => g a
  -> HalogenM s f g p o m (M.Map p a)
queryAll q =
  M.fromList <$> (traverse (\p -> map (Tuple p) (mkQuery p q)) =<< getSlots)

-- TODO: queryAll'

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
  :: forall f f' m o
   . Functor m
  => (f ~> f')
  -> (forall a. f' a -> Maybe (f a))
  -> Component f m o
  -> Component f' m o
transform reviewQ previewQ =
  unComponent \c ->
    mkComponent
      { initialState: c.initialState
      , render: bimap (map reviewQ) reviewQ <<< c.render
      , eval:
          maybe
            (HM.halt "prism failed in transform")
            (HM.hoistF reviewQ <<< c.eval)
              <<< previewQ
      , initializer: reviewQ <$> c.initializer
      , finalizer: reviewQ <$> c.finalizer
      , mkOrdBox: c.mkOrdBox
      }

-- | Transforms a `Component`'s types using a `ChildPath` definition.
transformChild
  :: forall f f' m p p' o
   . Functor m
  => ChildPath f f' p p'
  -> Component f m o
  -> Component f' m o
transformChild i = transform (injQuery i) (prjQuery i)

-- | Changes the component's `m` type. A use case for this would be to interpret
-- | some `Free` monad as `Aff` so the component can be used with `runUI`.
interpret
  :: forall f m m' o
   . Functor m'
  => (m ~> m')
  -> Component f m o
  -> Component f m' o
interpret nat =
  unComponent \c ->
    mkComponent
      { initialState: c.initialState
      , render: lmap (hoistSlotM nat) <<< c.render
      , eval: HM.hoistM nat <<< c.eval
      , initializer: c.initializer
      , finalizer: c.finalizer
      , mkOrdBox: c.mkOrdBox
      }
