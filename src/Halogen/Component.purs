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
  , queryAll'
  , transform
  , transformChild
  , interpret
  ) where

import Prelude

import Control.Monad.Free (liftF)

import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Const (Const)
import Data.Lazy (Lazy)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import Halogen.Component.ChildPath (ChildPath, injSlot, prjQuery, injQuery, prjSlot, cpI)
import Halogen.Data.OrdBox (OrdBox, mkOrdBox)
import Halogen.HTML.Core (HTML)
import Halogen.Query.ChildQuery (childQuery)
import Halogen.Query.HalogenM (HalogenM(..), HalogenF(..))
import Halogen.Query.HalogenM as HM

import Unsafe.Coerce (unsafeCoerce)

data ComponentSlot' h g m p i o = ComponentSlot p (Lazy (Component h g o m)) (o -> Maybe i)

data ComponentSlot (h :: * -> * -> *) (g :: * -> *) (m :: * -> *) p i

instance bifunctorSlotF :: Bifunctor (ComponentSlot h g m) where
  bimap f g = unComponentSlot \p ctor k -> mkComponentSlot (f p) ctor (map g <<< k)

instance functorSlotF :: Functor (ComponentSlot h g m p) where
  map = rmap

mkComponentSlot
  :: forall h g m p i o
   . p
  -> (Lazy (Component h g o m))
  -> (o -> Maybe i)
  -> ComponentSlot h g m p i
mkComponentSlot = unsafeCoerce ComponentSlot

unComponentSlot
  :: forall h g m p i r
   . (forall o. p -> Lazy (Component h g o m) -> (o -> Maybe i) -> r)
  -> ComponentSlot h g m p i
  -> r
unComponentSlot f cs =
  case unsafeCoerce cs of
    ComponentSlot p ctor k -> f p ctor k

hoistSlotM
  :: forall h g m m' p i
   . (Bifunctor h, Functor m')
  => (m ~> m')
  -> ComponentSlot h g m p i
  -> ComponentSlot h g m' p i
hoistSlotM nat = unComponentSlot \p ctor k ->
  mkComponentSlot p (map (interpret nat) ctor) k

--------------------------------------------------------------------------------

type ParentHTML f g p m = HTML (ComponentSlot HTML g m p (f Unit)) (f Unit)
type ParentDSL = HalogenM

type ComponentHTML f = HTML Void (f Unit)
type ComponentDSL s f = HalogenM s f (Const Void) Void

--------------------------------------------------------------------------------

type Component' h s f g p o m =
  { initialState :: s
  , render :: s -> h (ComponentSlot h g m p (f Unit)) (f Unit)
  , eval :: f ~> HalogenM s f g p o m
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  , mkOrdBox :: p -> OrdBox p
  }

--------------------------------------------------------------------------------

data Component (h :: * -> * -> *) (f :: * -> *) o (m :: * -> *)

mkComponent
  :: forall h s f g p o m
   . Component' h s f g p o m
  -> Component h f o m
mkComponent = unsafeCoerce

unComponent
  :: forall h f o m r
   . (forall s g p. Component' h s f g p o m -> r)
  -> Component h f o m
  -> r
unComponent = unsafeCoerce

--------------------------------------------------------------------------------

-- | A spec for a component.
type ComponentSpec h s f o m =
  { initialState :: s
  , render :: s -> h Void (f Unit)
  , eval :: f ~> ComponentDSL s f o m
  }

-- | Builds a self-contained component with no possible children.
component :: forall h s f o m. ComponentSpec h s f o m -> Component h f o m
component spec =
  lifecycleComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , initializer: Nothing
    , finalizer: Nothing
    }

-- | A spec for a component, including lifecycle inputs.
type LifecycleComponentSpec h s f o m i =
  { initialState :: s
  , render :: s -> h Void (f Unit)
  , eval :: f ~> ComponentDSL s f o m
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

-- | Builds a self-contained component with lifecycle inputs and no possible
-- | children.
lifecycleComponent
  :: forall h s f o m i
   . LifecycleComponentSpec h s f o m i
  -> Component h f o m
lifecycleComponent spec =
  mkComponent
    { initialState: spec.initialState
    , render: coeRender spec.render
    , eval: spec.eval
    , initializer: spec.initializer
    , finalizer: spec.finalizer
    , mkOrdBox: absurd
    }
  where
  coeRender
    :: (s -> h Void (f Unit))
    -> s
    -> h (ComponentSlot h (Const Void) m Void (f Unit)) (f Unit)
  coeRender = unsafeCoerce -- ≅ map (bimap absurd id)

type ParentComponentSpec h s f g p o m i =
  { initialState :: s
  , render :: s -> h (ComponentSlot h g m p (f Unit)) (f Unit)
  , eval :: f ~> HalogenM s f g p o m
  }

parentComponent
  :: forall h s f g p o m i
   . Ord p
  => ParentComponentSpec h s f g p o m i
  -> Component h f o m
parentComponent spec =
  mkComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , initializer: Nothing
    , finalizer: Nothing
    , mkOrdBox: mkOrdBox
    }

type ParentLifecycleComponentSpec h s f g p o m =
  { initialState :: s
  , render :: s -> h (ComponentSlot h g m p (f Unit)) (f Unit)
  , eval :: f ~> HalogenM s f g p o m
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

lifecycleParentComponent
  :: forall h s f g p o m
   . Ord p
  => ParentLifecycleComponentSpec h s f g p o m
  -> Component h f o m
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

checkSlot :: forall s f g p o m. p -> HalogenM s f g p o m Boolean
checkSlot p = HalogenM $ liftF $ CheckSlot p id

query
  :: forall s f g p o m a
   . (Applicative m, Eq p)
  => p
  -> g a
  -> HalogenM s f g p o m (Maybe a)
query p q = checkSlot p >>= if _ then Just <$> mkQuery p q else pure Nothing

query'
  :: forall s f g g' m p p' o a
   . (Applicative m, Eq p')
  => ChildPath g g' p p'
  -> p
  -> g a
  -> HalogenM s f g' p' o m (Maybe a)
query' path p q = query (injSlot path p) (injQuery path q)

queryAll
  :: forall s f g p o m a
   . (Applicative m, Ord p)
  => g a
  -> HalogenM s f g p o m (M.Map p a)
queryAll = queryAll' cpI

queryAll'
  :: forall s f g g' p p' o m a
   . (Applicative m, Ord p, Eq p')
  => ChildPath g g' p p'
  -> g a
  -> HalogenM s f g' p' o m (M.Map p a)
queryAll' path q = do
  slots <- L.mapMaybe (prjSlot path) <$> getSlots
  M.fromFoldable <$> (traverse (\p -> map (Tuple p) (mkQuery (injSlot path p) (injQuery path q))) slots)

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
  :: forall h f f' o m
   . (Bifunctor h, Functor m)
  => (f ~> f')
  -> (forall a. f' a -> Maybe (f a))
  -> Component h f o m
  -> Component h f' o m
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
  :: forall h f f' p p' o m
   . (Bifunctor h, Functor m)
  => ChildPath f f' p p'
  -> Component h f o m
  -> Component h f' o m
transformChild i = transform (injQuery i) (prjQuery i)

-- | Changes the component's `m` type. A use case for this would be to interpret
-- | some `Free` monad as `Aff` so the component can be used with `runUI`.
interpret
  :: forall h f o m m'
   . (Bifunctor h, Functor m')
  => (m ~> m')
  -> Component h f o m
  -> Component h f o m'
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
