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
  , hoist
  , ComponentSlot
  , mkComponentSlot
  , unComponentSlot
  , hoistSlot
  ) where

import Prelude

import Data.Bifunctor (class Bifunctor, lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..))

import Halogen.Data.OrdBox (OrdBox, mkOrdBox)
import Halogen.HTML.Core (HTML)
import Halogen.Query.HalogenM (HalogenM)
import Halogen.Query.HalogenM as HM

import Unsafe.Coerce (unsafeCoerce)

-- | The "public" type for a component, with details of the component internals
-- | existentially hidden.
-- |
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `f` is the query algebra
-- | - `i` is the input value type that will be mapped to an `f` whenever the
-- |       parent of this component renders
-- | - `o` is the type for the component's output messages
-- | - `m` is the monad used for non-component-state effects
data Component (h :: Type -> Type -> Type) (f :: Type -> Type) i o (m :: Type -> Type)

-- | Makes a `Component` from a `Component'`, existentially hiding details about
-- | the component's state and potential children.
mkComponent
  :: forall h s f g p i o m
   . Component' h s f g p i o m
  -> Component h f i o m
mkComponent = unsafeCoerce

-- | Exposes the inner details of a component to a function to produce a new
-- | result. The inner details will not be allowed to be revealed in the result
-- | of the function - the compiler will complain about an escaped skolem.
unComponent
  :: forall h f i o m r
   . (forall s g p. Component' h s f g p i o m -> r)
  -> Component h f i o m
  -> r
unComponent = unsafeCoerce

-- | The "private" type for a component.
-- |
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `s` is the component's state
-- | - `f` is the query algebra for the component itself
-- | - `g` is the query algebra for child components
-- | - `p` is the slot type for addressing child components
-- | - `i` is the input value type that will be mapped to an `f` whenever the
-- |       parent of this component renders
-- | - `o` is the type for the component's output messages
-- | - `m` is the monad used for non-component-state effects
type Component' h s f g p i o m =
  { initialState :: i -> s
  , render :: s -> h (ComponentSlot h g m p (f Unit)) (f Unit)
  , eval :: f ~> HalogenM s f g p o m
  , receiver :: i -> Maybe (f Unit)
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  , mkOrdBox :: p -> OrdBox p
  }

-- | A spec for a component with no possible children.
-- |
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `s` is the component's state
-- | - `f` is the query algebra
-- | - `i` is the input value type that will be mapped to an `f` whenever the
-- |       parent of this component renders
-- | - `o` is the type for the component's output messages
-- | - `m` is the monad used for non-component-state effects
type ComponentSpec h s f i o m =
  { initialState :: i -> s
  , render :: s -> h Void (f Unit)
  , eval :: f ~> ComponentDSL s f o m
  , receiver :: i -> Maybe (f Unit)
  }

-- | A convenience synonym for the output type of a `render` function, for a
-- | childless component that renders HTML.
type ComponentHTML f = HTML Void (f Unit)

-- | A synonym for `HalogenM` with some type parameters populated that are not
-- | relevant for childless components.
type ComponentDSL s f = HalogenM s f (Const Void) Void

-- | Builds a component with no possible children.
component
  :: forall h s f i o m
   . Bifunctor h
  => ComponentSpec h s f i o m
  -> Component h f i o m
component spec =
  lifecycleComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , receiver: spec.receiver
    , initializer: Nothing
    , finalizer: Nothing
    }

-- | A spec for a component with no possible children, including lifecycle
-- | inputs.
-- |
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `s` is the component's state
-- | - `f` is the query algebra
-- | - `i` is the input value type that will be mapped to an `f` whenever the
-- |       parent of this component renders
-- | - `o` is the type for the component's output messages
-- | - `m` is the monad used for non-component-state effects
type LifecycleComponentSpec h s f i o m =
  { initialState :: i -> s
  , render :: s -> h Void (f Unit)
  , eval :: f ~> ComponentDSL s f o m
  , receiver :: i -> Maybe (f Unit)
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

-- | Builds a component with lifecycle inputs and no possible children.
lifecycleComponent
  :: forall h s f i o m
   . Bifunctor h
  => LifecycleComponentSpec h s f i o m
  -> Component h f i o m
lifecycleComponent spec =
  mkComponent
    { initialState: spec.initialState
    , render: coeRender spec.render
    , eval: spec.eval
    , receiver: spec.receiver
    , initializer: spec.initializer
    , finalizer: spec.finalizer
    , mkOrdBox
    }
  where
  coeRender
    :: (s -> h Void (f Unit))
    -> s
    -> h (ComponentSlot h (Const Void) m Void (f Unit)) (f Unit)
  coeRender = unsafeCoerce -- ≅ map (lmap absurd)

-- | A spec for a component.
-- |
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `s` is the component's state
-- | - `f` is the query algebra for the component itself
-- | - `g` is the query algebra for child components
-- | - `p` is the slot type for addressing child components
-- | - `o` is the type for the component's output messages
-- | - `m` is the monad used for non-component-state effects
type ParentComponentSpec h s f g p i o m =
  { initialState :: i -> s
  , render :: s -> h (ComponentSlot h g m p (f Unit)) (f Unit)
  , eval :: f ~> HalogenM s f g p o m
  , receiver :: i -> Maybe (f Unit)
  }

-- | A convenience synonym for the output type of a `render` function, for a
-- | parent component that renders HTML.
type ParentHTML f g p m = HTML (ComponentSlot HTML g m p (f Unit)) (f Unit)

-- | A synonym for just `HalogenM`. Provided for consistency with `ComponentDSL`
-- | in the non-parent-component case.
type ParentDSL = HalogenM

-- | Builds a component that allows for children.
parentComponent
  :: forall h s f g p i o m
   . Ord p
  => ParentComponentSpec h s f g p i o m
  -> Component h f i o m
parentComponent spec =
  mkComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , receiver: spec.receiver
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
type ParentLifecycleComponentSpec h s f g p i o m =
  { initialState :: i -> s
  , render :: s -> h (ComponentSlot h g m p (f Unit)) (f Unit)
  , eval :: f ~> HalogenM s f g p o m
  , receiver :: i -> Maybe (f Unit)
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

-- | Builds a component with lifecycle inputs that allows for children.
lifecycleParentComponent
  :: forall h s f g p i o m
   . Ord p
  => ParentLifecycleComponentSpec h s f g p i o m
  -> Component h f i o m
lifecycleParentComponent spec =
  mkComponent
    { initialState: spec.initialState
    , render: spec.render
    , eval: spec.eval
    , receiver: spec.receiver
    , initializer: spec.initializer
    , finalizer: spec.finalizer
    , mkOrdBox
    }

-- | Changes the component's `m` type. A use case for this would be to interpret
-- | some `Free` monad as `Aff` so the component can be used with `runUI`.
hoist
  :: forall h f i o m m'
   . Bifunctor h
  => Functor m'
  => (m ~> m')
  -> Component h f i o m
  -> Component h f i o m'
hoist nat =
  unComponent \c ->
    mkComponent
      { initialState: c.initialState
      , render: lmap (hoistSlot nat) <<< c.render
      , eval: HM.hoist nat <<< c.eval
      , receiver: c.receiver
      , initializer: c.initializer
      , finalizer: c.finalizer
      , mkOrdBox: c.mkOrdBox
      }

--------------------------------------------------------------------------------

data ComponentSlot' h z g m p j q o = ComponentSlot p (Component h z j o m) j (j -> Maybe (g Unit)) (o -> Maybe q) (forall x. g x -> Maybe (z x))

data ComponentSlot (h :: Type -> Type -> Type) (g :: Type -> Type) (m :: Type -> Type) p q

instance bifunctorSlotF :: Bifunctor (ComponentSlot h g m) where
  bimap f g = unComponentSlot \p ctor input inputQuery outputQuery projQuery ->
    mkComponentSlot (f p) ctor input inputQuery (map g <<< outputQuery) projQuery

instance functorSlotF :: Functor (ComponentSlot h g m p) where
  map f = unComponentSlot \p ctor j g h i ->
    mkComponentSlot p ctor j g (map f <<< h) i

mkComponentSlot
  :: forall h g z m p j q o
   . p
  -> (Component h z j o m)
  -> j
  -> (j -> Maybe (g Unit))
  -> (o -> Maybe q)
  -> (forall x. g x -> Maybe (z x))
  -> ComponentSlot h g m p q
mkComponentSlot = unsafeCoerce ComponentSlot

unComponentSlot
  :: forall h g m p q r
   . (forall z j o. p -> Component h z j o m -> j -> (j -> Maybe (g Unit)) -> (o -> Maybe q) -> (forall x. g x -> Maybe (z x)) -> r)
  -> ComponentSlot h g m p q
  -> r
unComponentSlot f cs =
  case unsafeCoerce cs of
    ComponentSlot p ctor input inputQuery outputQuery projQuery ->
      f p ctor input inputQuery outputQuery projQuery

hoistSlot
  :: forall h g m m' p q
   . Bifunctor h
  => Functor m'
  => (m ~> m')
  -> ComponentSlot h g m p q
  -> ComponentSlot h g m' p q
hoistSlot nat = unComponentSlot \p ctor input inputQuery outputQuery projQuery ->
  mkComponentSlot p (hoist nat ctor) input inputQuery outputQuery projQuery
