module Halogen.Component
  ( Component
  , ComponentSpec
  , component
  , ComponentSpec'
  , component'
  , unComponent
  , hoist
  , ComponentSlot'
  , ComponentSlot
  , mkComponentSlot
  , mkComponentSlot'
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
-- |       this component renders
-- | - `o` is the type of messages the component can raise
-- | - `m` is the effect monad used during evaluation
data Component (h :: Type -> Type -> Type) (f :: Type -> Type) i o (m :: Type -> Type)

-- | The spec for a component.
-- |
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `s` is the component's state
-- | - `f` is the query algebra; the requests that can be made of the component
-- | - `ps` is the set of slots for addressing child components
-- | - `i` is the input value that will be received when the parent of
-- |       this component renders
-- | - `o` is the type of messages the component can raise
-- | - `m` is the effect monad used during evaluation
type ComponentSpec h s f ps i o m =
  { initialState :: i -> s
  , render :: s -> h (ComponentSlot h ps m (f Unit)) (f Unit)
  , eval :: f ~> HalogenM' s (f Unit) ps o m
  , receiver :: i -> Maybe (f Unit)
  , initializer :: Maybe (f Unit)
  , finalizer :: Maybe (f Unit)
  }

-- | Builds a component from a `ComponentSpec`.
component
  :: forall h s f ps i o m
   . ComponentSpec h s f ps i o m
  -> Component h f i o m
component = component' <<< go
  where
    go :: ComponentSpec h s f ps i o m -> ComponentSpec' h s f (f Unit) ps i o m
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

-- | An alternative formulation of the spec for a component.
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
-- | - `h` is the type that will be rendered by the component, usually `HTML`
-- | - `s` is the component's state
-- | - `f` is the query algebra; the requests that can be made of the component
-- | - `act` is the type of actions; messages internal to the component that
-- |         can be evaluated
-- | - `ps` is the set of slots for addressing child components
-- | - `i` is the input value that will be received when the parent of
-- |       this component renders
-- | - `o` is the type of messages the component can raise
-- | - `m` is the effect monad used during evaluation
type ComponentSpec' h s f act ps i o m =
  { initialState :: i -> s
  , render :: s -> h (ComponentSlot h ps m act) act
  , eval :: HalogenQ f act i ~> HalogenM' s act ps o m
  }

-- | Builds a component from a `ComponentSpec'`.
component'
  :: forall h s f g ps i o m
   . ComponentSpec' h s f g ps i o m
  -> Component h f i o m
component' = unsafeCoerce

-- | Exposes the inner details of a component to a function to produce a new
-- | result. The hidden details will not be allowed to be revealed in the result
-- | of the function - the compiler will complain about an escaped skolem if
-- | this is attempted.
unComponent
  :: forall h f i o m r
   . (forall s act ps. ComponentSpec' h s f act ps i o m -> r)
  -> Component h f i o m
  -> r
unComponent = unsafeCoerce

-- | Changes the component's `m` type. A use case for this would be to interpret
-- | some `Free` monad as `Aff` so the component can be used with `runUI`.
hoist
  :: forall h f i o m m'
   . Bifunctor h
  => Functor m'
  => (m ~> m')
  -> Component h f i o m
  -> Component h f i o m'
hoist nat = unComponent \c ->
  component'
    { initialState: c.initialState
    , render: lmap (hoistSlot nat) <<< c.render
    , eval: HM.hoist nat <<< c.eval
    }

--------------------------------------------------------------------------------

type ComponentSlot' h f i o ps m a =
  { get :: forall slot. SlotStorage ps slot -> Maybe (slot f o)
  , pop :: forall slot. SlotStorage ps slot -> Maybe (Tuple (slot f o) (SlotStorage ps slot))
  , set :: forall slot. slot f o -> SlotStorage ps slot -> SlotStorage ps slot
  , component :: Component h f i o m
  , input :: forall act. HalogenQ f act i Unit
  , output :: o -> Maybe a
  }

data ComponentSlot (h :: Type -> Type -> Type) (ps :: # Type) (m :: Type -> Type) a

instance functorComponentSlot :: Functor (ComponentSlot h ps m) where
  map f = unComponentSlot \slot -> mkComponentSlot' $ slot { output = map f <$> slot.output }

mkComponentSlot
  :: forall h sym px ps f i o p m a
   . Row.Cons sym (Slot f o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> Component h f i o m
  -> i
  -> (o -> Maybe a)
  -> ComponentSlot h ps m a
mkComponentSlot sym p comp input output =
  mkComponentSlot' { get, pop, set, component: comp, input: Receive input unit, output }
  where
  get :: forall slot. SlotStorage ps slot -> Maybe (slot f o)
  get = Slot.lookup sym p

  pop :: forall slot. SlotStorage ps slot -> Maybe (Tuple (slot f o) (SlotStorage ps slot))
  pop = Slot.pop sym p

  set :: forall slot. slot f o -> SlotStorage ps slot -> SlotStorage ps slot
  set = Slot.insert sym p

mkComponentSlot'
  :: forall h f i o ps m a
   . ComponentSlot' h f i o ps m a
  -> ComponentSlot h ps m a
mkComponentSlot' = unsafeCoerce

unComponentSlot
  :: forall h ps m a r
   . (forall f i o. ComponentSlot' h f i o ps m a -> r)
  -> ComponentSlot h ps m a
  -> r
unComponentSlot = unsafeCoerce

hoistSlot
  :: forall h m m' ps a
   . Bifunctor h
  => Functor m'
  => (m ~> m')
  -> ComponentSlot h ps m a
  -> ComponentSlot h ps m' a
hoistSlot nat = unComponentSlot \slot ->
  mkComponentSlot' $ slot { component = hoist nat slot.component }
