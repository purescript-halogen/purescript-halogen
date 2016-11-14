module Halogen.Component
  ( Component
  , Component'
  , mkComponent
  , unComponent
  , ComponentSpec
  , ComponentHTML
  , ComponentDSL
  , component
  , LifecycleComponentSpec
  , lifecycleComponent
  , ParentComponentSpec
  , parentComponent
  , ParentHTML
  , ParentDSL
  , ParentLifecycleComponentSpec
  , lifecycleParentComponent
  , transform
  , transformChild
  , interpret
  , ComponentSlot
  , mkComponentSlot
  , unComponentSlot
  ) where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Const (Const)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe(..), maybe)

import Halogen.Component.ChildPath (ChildPath, prjQuery, injQuery)
import Halogen.Data.OrdBox (OrdBox, mkOrdBox)
import Halogen.HTML.Core (HTML)
import Halogen.Query.HalogenM (HalogenM, halt, hoistF, hoistM)

import Unsafe.Coerce (unsafeCoerce)

-- | The "public" type for a component, with details of the component internals
-- | existentially hidden.
-- |
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `f` is the query algebra
-- | - `o` is the type for the component's output messages
-- | - `m` is the monad used for non-component-state effects
data Component (h :: * -> * -> *) (f :: * -> *) o (m :: * -> *)

-- | Makes a `Component` from a `Component'`, existentially hiding details about
-- | the component's state and potential children.
mkComponent
  :: forall h s f g p o m
   . Component' h s f g p o m
  -> Component h f o m
mkComponent = unsafeCoerce

-- | Exposes the inner details of a component to a function to produce a new
-- | result. The inner details will not be allowed to be revealed in the result
-- | of the function - the compiler will complain about an escaped skolem.
unComponent
  :: forall h f o m r
   . (forall s g p. Component' h s f g p o m -> r)
  -> Component h f o m
  -> r
unComponent = unsafeCoerce

-- | The "private" type for a component.
-- |
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `s` is the component's state
-- | - `f` is the query algebra for the component itself
-- | - `g` is the query algebra for child components
-- | - `p` is the slot type for addressing child components
-- | - `o` is the type for the component's output messages
-- | - `m` is the monad used for non-component-state effects
type Component' h s f g p o m =
  { initialState :: s
  , render :: s -> h (ComponentSlot h g m p (f Unit)) (f Unit)
  , eval :: f ~> HalogenM s f g p o m
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  , mkOrdBox :: p -> OrdBox p
  }

-- | A spec for a component with no possible children.
-- |
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `s` is the component's state
-- | - `f` is the query algebra
-- | - `o` is the type for the component's output messages
-- | - `m` is the monad used for non-component-state effects
type ComponentSpec h s f o m =
  { initialState :: s
  , render :: s -> h Void (f Unit)
  , eval :: f ~> ComponentDSL s f o m
  }

-- | A convenience synonym for the output type of a `render` function, for a
-- | childless component that renders HTML.
type ComponentHTML f = HTML Void (f Unit)

-- | A synonym for `HalogenM` with some type parameters populated that are not
-- | relevant for childless components.
type ComponentDSL s f = HalogenM s f (Const Void) Void

-- | Builds a component with no possible children.
component :: forall h s f o m. ComponentSpec h s f o m -> Component h f o m
component spec =
  lifecycleComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , initializer: Nothing
    , finalizer: Nothing
    }

-- | A spec for a component with no possible children, including lifecycle
-- | inputs.
-- |
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `s` is the component's state
-- | - `f` is the query algebra
-- | - `o` is the type for the component's output messages
-- | - `m` is the monad used for non-component-state effects
type LifecycleComponentSpec h s f o m =
  { initialState :: s
  , render :: s -> h Void (f Unit)
  , eval :: f ~> ComponentDSL s f o m
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

-- | Builds a component with lifecycle inputs and no possible children.
lifecycleComponent
  :: forall h s f o m
   . LifecycleComponentSpec h s f o m
  -> Component h f o m
lifecycleComponent spec =
  mkComponent
    { initialState: spec.initialState
    , render: coeRender spec.render
    , eval: spec.eval
    , initializer: spec.initializer
    , finalizer: spec.finalizer
    , mkOrdBox
    }
  where
  coeRender
    :: (s -> h Void (f Unit))
    -> s
    -> h (ComponentSlot h (Const Void) m Void (f Unit)) (f Unit)
  coeRender = unsafeCoerce -- ≅ map (bimap absurd id)

-- | A spec for a component.
-- |
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `s` is the component's state
-- | - `f` is the query algebra for the component itself
-- | - `g` is the query algebra for child components
-- | - `p` is the slot type for addressing child components
-- | - `o` is the type for the component's output messages
-- | - `m` is the monad used for non-component-state effects
type ParentComponentSpec h s f g p o m =
  { initialState :: s
  , render :: s -> h (ComponentSlot h g m p (f Unit)) (f Unit)
  , eval :: f ~> HalogenM s f g p o m
  }

-- | A convenience synonym for the output type of a `render` function, for a
-- | parent component that renders HTML.
type ParentHTML f g p m = HTML (ComponentSlot HTML g m p (f Unit)) (f Unit)

-- | A synonym for just `HalogenM`. Provided for consistency with `ComponentDSL`
-- | in the non-parent-component case.
type ParentDSL = HalogenM

-- | Builds a component that allows for children.
parentComponent
  :: forall h s f g p o m
   . Ord p
  => ParentComponentSpec h s f g p o m
  -> Component h f o m
parentComponent spec =
  mkComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , initializer: Nothing
    , finalizer: Nothing
    , mkOrdBox
    }

-- | A spec for a parent component, including lifecycle inputs.
-- |
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `s` is the component's state
-- | - `f` is the query algebra for the component itself
-- | - `g` is the query algebra for child components
-- | - `p` is the slot type for addressing child components
-- | - `o` is the type for the component's output messages
-- | - `m` is the monad used for non-component-state effects
type ParentLifecycleComponentSpec h s f g p o m =
  { initialState :: s
  , render :: s -> h (ComponentSlot h g m p (f Unit)) (f Unit)
  , eval :: f ~> HalogenM s f g p o m
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

-- | Builds a component with lifecycle inputs that allows for children.
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
    , mkOrdBox
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
            (halt "prism failed in transform")
            (hoistF reviewQ <<< c.eval)
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
      , eval: hoistM nat <<< c.eval
      , initializer: c.initializer
      , finalizer: c.finalizer
      , mkOrdBox: c.mkOrdBox
      }

--------------------------------------------------------------------------------

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
