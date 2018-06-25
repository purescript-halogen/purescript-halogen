module Halogen.Component
  ( Component
  , ComponentArgs
  , component
  , ComponentSpec
  , mkComponent
  , unComponent
  , hoist
  , ComponentSlot
  , componentSlot
  , ComponentSlotSpec
  , mkComponentSlot
  , unComponentSlot
  , hoistSlot
  ) where

import Prelude

import Data.Bifunctor (class Bifunctor, lmap)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Tuple (Tuple)
import Halogen.Data.Slot (Slot, SlotStorage)
import Halogen.Data.Slot as Slot
import Halogen.Query.HalogenM (HalogenM')
import Halogen.Query.HalogenM as HM
import Halogen.Query.HalogenQ (HalogenQ(..))
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

-- | The "public" type for a component, with details of the component internals
-- | existentially hidden.
-- |
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `f` is the query algebra; the requests that can be made of the component
-- | - `i` is the input value that will be received when the parent of
-- |   this component renders
-- | - `o` is the type of messages the component can raise
-- | - `m` is the effect monad used during evaluation
data Component (h :: Type -> Type -> Type) (f :: Type -> Type) i o (m :: Type -> Type)

-- | The argument record for the `component` function.
-- |
-- | The type variables involved:
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `s` is the component's state
-- | - `f` is the query algebra; the requests that can be made of the component
-- | - `ps` is the set of slots for addressing child components
-- | - `i` is the input value that will be received when the parent of
-- |    this component renders
-- | - `o` is the type of messages the component can raise
-- | - `m` is the effect monad used during evaluation
-- |
-- | The values in the record:
-- | - `initialState` is a function that accepts an input value and produces the
-- |   state the component will start with. If the input value is unused
-- |   (`Unit`), or irrelevant to the state construction, this will often be
-- |   `const ?someInitialStateValue`.
-- | - `render` is a function that accepts the component's current state and
-- |   produces a value to render (`HTML` usually). The rendered output can
-- |   raise `f Unit` queries that the component will evaluate.
-- | - `eval` handles runing queries to change the state of the component, or
-- |   perform some effect, or return some information about the component's
-- |   state.
-- | - `receiever` is a function that maps an input value to a query. Every time
-- |   the input value changes this query will be passed through to `eval`.
-- |   `const Nothing` can be used here if no action needs to be taken.
-- | - `initializer` specifies an optional action-style query to raise on the
-- |   component when it is first constructed. This allows the component to
-- |   perfom effects that may be necessary to set up the component.
-- | - `finalizer` specifies an optional action-style query to raise on the
-- |   component when it is being removed. This allows the component to perfom
-- |   effects that may be necessary to clean up after the component. This is
-- |   not commonly necessary - subscriptions are automatically ended internally
-- |   by Halogen, it's only required for components that manipulate resources
-- |   outside of Halogen's reach.
type ComponentArgs h s f ps i o m =
  { initialState :: i -> s
  , render :: s -> h (ComponentSlot h ps m (f Unit)) (f Unit)
  , eval :: f ~> HalogenM' s (f Unit) ps o m
  , receiver :: i -> Maybe (f Unit)
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

-- | Builds a [`Component`](#t:Component) from a [`ComponentArgs`](#t:ComponentArgs)
-- | record.
component
  :: forall h s f ps i o m
   . ComponentArgs h s f ps i o m
  -> Component h f i o m
component = mkComponent <<< go
  where
    go :: ComponentArgs h s f ps i o m -> ComponentSpec h s f (f Unit) ps i o m
    go spec =
      { initialState: spec.initialState
      , render: spec.render
      , eval: case _ of
          Initialize a -> traverse_ spec.eval spec.initializer $> a
          Finalize a -> traverse_ spec.eval spec.finalizer $> a
          Receive i a -> traverse_ spec.eval (spec.receiver i) $> a
          Handle fa a -> spec.eval fa $> a
          Request fa -> spec.eval fa
      }

-- | The spec for a component. This differs from [`ComponentArgs`](#t:ComponentArgs)
-- | as it is the representation used by Halogen - the [`component`](#v:component)
-- | function translates a [`ComponentArgs`](#t:ComponentArgs) record into a
-- | `ComponentSpec`.
-- |
-- | Instead of accepting a number of properties that map things to queries,
-- | these cases are instead handled by pattern matching on a `HalogenQ` algebra
-- | that `eval` takes as an argument.
-- |
-- | Components defined with this spec can also separate the public query
-- | algebra for requests from actions that can be handled internally. The other
-- | spec formulation requires these actions to be of type `f Unit`, so in that
-- | case the query algebra must also contain the actions the component can
-- | raise internally.
-- |
-- | The type variables involved:
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `s` is the component's state
-- | - `f` is the query algebra; the requests that can be made of the component
-- | - `act` is the type of actions; messages internal to the component that
-- |   can be evaluated
-- | - `ps` is the set of slots for addressing child components
-- | - `i` is the input value that will be received when the parent of
-- |   this component renders
-- | - `o` is the type of messages the component can raise
-- | - `m` is the effect monad used during evaluation
-- |
-- | The values in the record:
-- | - `initialState` is a function that accepts an input value and produces the
-- |   state the component will start with. If the input value is unused
-- |   (`Unit`), or irrelevant to the state construction, this will often be
-- |   `const ?someInitialStateValue`.
-- | - `render` is a function that accepts the component's current state and
-- |   produces a value to render (`HTML` usually). The rendered output can
-- |   raise actions that will be handled in `eval`.
-- | - `eval` is a function that handles the `HalogenQ` algebra that deals with
-- |   component lifecycle, handling actions, and responding to requests.
type ComponentSpec h s f act ps i o m =
  { initialState :: i -> s
  , render :: s -> h (ComponentSlot h ps m act) act
  , eval :: HalogenQ f act i ~> HalogenM' s act ps o m
  }

-- | Constructs a [`Component`](#t:Component) from a [`ComponentSpec`](#t:ComponentSpec).
mkComponent
  :: forall h s f act ps i o m
   . ComponentSpec h s f act ps i o m
  -> Component h f i o m
mkComponent = unsafeCoerce

-- | Exposes the inner details of a [`Component`](#t:Component) to a function
-- | to produce a new result.
-- |
-- | The hidden details will not be allowed to be revealed in the result
-- | of the function - if any of the hidden types (state, action, set of slots)
-- | appear in the result, the compiler will complain about an escaped skolem.
unComponent
  :: forall h f i o m r
   . (forall s act ps. ComponentSpec h s f act ps i o m -> r)
  -> Component h f i o m
  -> r
unComponent = unsafeCoerce

-- | Changes the [`Component`](#t:Component)'s `m` type. A use case for this
-- | might be to interpret some `Free` monad as `Aff` so the component can be
-- | used with `runUI`.
hoist
  :: forall h f i o m m'
   . Bifunctor h
  => Functor m'
  => (m ~> m')
  -> Component h f i o m
  -> Component h f i o m'
hoist nat = unComponent \c ->
  mkComponent
    { initialState: c.initialState
    , render: lmap (hoistSlot nat) <<< c.render
    , eval: HM.hoist nat <<< c.eval
    }

-- | A slot for a child component in a component's rendered content.
data ComponentSlot (h :: Type -> Type -> Type) (ps :: # Type) (m :: Type -> Type) a

instance functorComponentSlot :: Functor (ComponentSlot h ps m) where
  map f = unComponentSlot \slot ->
    mkComponentSlot $ slot { output = map f <$> slot.output }

-- | Constructs a [`ComponentSlot`](#t:ComponentSlot).
-- |
-- | Takes:
-- | - the slot address label
-- | - the slot address index
-- | - the component for the slot
-- | - the input value to pass to the component
-- | - a function mapping outputs from the component to a query in the parent
componentSlot
  :: forall h sym px ps f i o p m act
   . Row.Cons sym (Slot f o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> Component h f i o m
  -> i
  -> (o -> Maybe act)
  -> ComponentSlot h ps m act
componentSlot sym p comp input output =
  mkComponentSlot
    { get: Slot.lookup sym p
    , pop: Slot.pop sym p
    , set: Slot.insert sym p
    , component: comp
    , input: Receive input unit
    , output
    }

-- | The internal representation used for a [`ComponentSlot`](#t:ComponentSlot).
type ComponentSlotSpec h f i o ps m act =
  { get :: forall slot. SlotStorage ps slot -> Maybe (slot f o)
  , pop :: forall slot. SlotStorage ps slot -> Maybe (Tuple (slot f o) (SlotStorage ps slot))
  , set :: forall slot. slot f o -> SlotStorage ps slot -> SlotStorage ps slot
  , component :: Component h f i o m
  , input :: forall a. HalogenQ f a i Unit
  , output :: o -> Maybe act
  }

-- | Constructs [`ComponentSlot`](#t:ComponentSlot) from a [`ComponentSlotSpec`](#t:ComponentSlotSpec).
mkComponentSlot
  :: forall h f i o ps m act
   . ComponentSlotSpec h f i o ps m act
  -> ComponentSlot h ps m act
mkComponentSlot = unsafeCoerce

-- | Exposes the inner details of a [`ComponentSlot`](#t:ComponentSlot) to a
-- | function to produce a new result.
-- |
-- | The hidden details will not be allowed to be revealed in the result
-- | of the function - if any of the hidden types (state, action, set of slots)
-- | appear in the result, the compiler will complain about an escaped skolem.
unComponentSlot
  :: forall h ps m act r
   . (forall f i o. ComponentSlotSpec h f i o ps m act -> r)
  -> ComponentSlot h ps m act
  -> r
unComponentSlot = unsafeCoerce

-- | Changes the [`ComponentSlot`](#t:ComponentSlot)'s `m` type.
hoistSlot
  :: forall h m m' ps act
   . Bifunctor h
  => Functor m'
  => (m ~> m')
  -> ComponentSlot h ps m act
  -> ComponentSlot h ps m' act
hoistSlot nat = unComponentSlot \slot ->
  mkComponentSlot $ slot { component = hoist nat slot.component }
