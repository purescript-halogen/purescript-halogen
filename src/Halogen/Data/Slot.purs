module Halogen.Data.Slot
  ( Slot
  , SlotStorage
  , empty
  , lookup
  , insert
  , pop
  , slots
  , foreachSlot
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Alternate (Alternate(..))
import Data.Newtype (un)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Halogen.Data.OrdBox (OrdBox, mkOrdBox, unOrdBox)
import Prim.Row as Row
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Any :: Type

-- | A type which records the queries, output messages, and slot identifier for
-- | a particular slot (ie. a location in HTML where a component is rendered).
-- | For example:
-- |
-- | ```purescript
-- | type ButtonSlot slot = Slot Button.Query Button.Output slot
-- |
-- | -- A component using this slot type can have one type of child component,
-- | -- which supports `Button.Query` queries, `Button.Output` outputs, and
-- | -- which can be uniquely identified by an integer.
-- | type Slots = ( button :: ButtonSlot Int )
-- | ```
-- |
-- | - `query` represents the requests that can be made of this component
-- | - `output` represents the output messages that can be raised by this component
-- | - `slot` represents the unique identifier for this component
data Slot :: (Type -> Type) -> Type -> Type -> Type
data Slot (query :: Type -> Type) output slot

newtype SlotStorage (slots :: Row Type) (slot :: (Type -> Type) -> Type -> Type) =
  SlotStorage (Map (Tuple String (OrdBox Any)) Any)

empty :: forall slots slot. SlotStorage slots slot
empty = SlotStorage Map.empty

lookup
  :: forall sym px slots slot query output s
   . Row.Cons sym (Slot query output s) px slots
  => IsSymbol sym
  => Ord s
  => Proxy sym
  -> s
  -> SlotStorage slots slot
  -> Maybe (slot query output)
lookup sym key (SlotStorage m) =
  coerceSlot (Map.lookup (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox key))) m)
  where
  coerceSlot :: Maybe Any -> Maybe (slot query output)
  coerceSlot = unsafeCoerce

  coerceBox :: OrdBox s -> OrdBox Any
  coerceBox = unsafeCoerce

pop
  :: forall sym px slots slot query output s
   . Row.Cons sym (Slot query output s) px slots
  => IsSymbol sym
  => Ord s
  => Proxy sym
  -> s
  -> SlotStorage slots slot
  -> Maybe (Tuple (slot query output) (SlotStorage slots slot))
pop sym key (SlotStorage m) =
  coercePop (Map.pop (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox key))) m)
  where
  coercePop :: Maybe (Tuple Any (Map (Tuple String (OrdBox Any)) Any)) -> Maybe (Tuple (slot query output) (SlotStorage slots slot))
  coercePop = unsafeCoerce

  coerceBox :: OrdBox s -> OrdBox Any
  coerceBox = unsafeCoerce

insert
  :: forall sym px slots slot query output s
   . Row.Cons sym (Slot query output s) px slots
  => IsSymbol sym
  => Ord s
  => Proxy sym
  -> s
  -> slot query output
  -> SlotStorage slots slot
  -> SlotStorage slots slot
insert sym key val (SlotStorage m) =
  SlotStorage (Map.insert (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox key))) (coerceVal val) m)
  where
  coerceBox :: OrdBox s -> OrdBox Any
  coerceBox = unsafeCoerce

  coerceVal :: slot query output -> Any
  coerceVal = unsafeCoerce

slots
  :: forall sym px slots slot query output s
   . Row.Cons sym (Slot query output s) px slots
  => IsSymbol sym
  => Ord s
  => Proxy sym
  -> SlotStorage slots slot
  -> Map s (slot query output)
slots sym (SlotStorage m) = un Alternate $ Map.foldSubmap Nothing Nothing go m
  where
  key = reflectSymbol sym

  go (Tuple key' ob) val
    | key == key' = Alternate $ Map.singleton (unOrdBox (coerceBox ob)) (coerceVal val)
    | otherwise = Alternate Map.empty

  coerceBox :: OrdBox Any -> OrdBox s
  coerceBox = unsafeCoerce

  coerceVal :: Any -> slot query output
  coerceVal = unsafeCoerce

foreachSlot
  :: forall m slots slot
   . Applicative m
  => SlotStorage slots slot
  -> (forall query output. slot query output -> m Unit)
  -> m Unit
foreachSlot (SlotStorage m) k = traverse_ (k <<< coerceVal) m
  where
  coerceVal :: forall query output. Any -> slot query output
  coerceVal = unsafeCoerce
