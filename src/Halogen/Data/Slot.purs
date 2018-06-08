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
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..))
import Halogen.Data.OrdBox (OrdBox, mkOrdBox, unOrdBox)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

foreign import data Any :: Type

data Slot (g :: Type -> Type) i o p

newtype SlotStorage (partitions :: # Type) (slot :: (Type -> Type) -> Type -> Type -> Type) =
  SlotStorage (Map (Tuple String (OrdBox Any)) Any)

empty :: forall ps slot. SlotStorage ps slot
empty = SlotStorage Map.empty

lookup
  :: forall sym px ps slot g i o p
   . Row.Cons sym (Slot g i o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> SlotStorage ps slot
  -> Maybe (slot g i o)
lookup sym key (SlotStorage m) =
  coerceSlot (Map.lookup (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox key))) m)
  where
  coerceSlot :: Maybe Any -> Maybe (slot g i o)
  coerceSlot = unsafeCoerce

  coerceBox :: OrdBox p -> OrdBox Any
  coerceBox = unsafeCoerce

pop
  :: forall sym px ps slot g i o p
   . Row.Cons sym (Slot g i o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> SlotStorage ps slot
  -> Maybe (Tuple (slot g i o) (SlotStorage ps slot))
pop sym key (SlotStorage m) =
  coercePop (Map.pop (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox key))) m)
  where
  coercePop :: Maybe (Tuple Any (Map (Tuple String (OrdBox Any)) Any)) -> Maybe (Tuple (slot g i o) (SlotStorage ps slot))
  coercePop = unsafeCoerce

  coerceBox :: OrdBox p -> OrdBox Any
  coerceBox = unsafeCoerce

insert
  :: forall sym px ps slot g i o p
   . Row.Cons sym (Slot g i o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> slot g i o
  -> SlotStorage ps slot
  -> SlotStorage ps slot
insert sym key val (SlotStorage m) =
  SlotStorage (Map.insert (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox key))) (coerceVal val) m)
  where
  coerceBox :: OrdBox p -> OrdBox Any
  coerceBox = unsafeCoerce

  coerceVal :: slot g i o -> Any
  coerceVal = unsafeCoerce

slots
  :: forall sym px ps slot g i o p
   . Row.Cons sym (Slot g i o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> SlotStorage ps slot
  -> Map p (slot g i o)
slots sym (SlotStorage m) = Map.foldSubmap Nothing Nothing go m
  where
  key = reflectSymbol sym

  go (Tuple key' ob) val
    | key == key' = Map.singleton (unOrdBox (coerceBox ob)) (coerceVal val)
    | otherwise = mempty

  coerceBox :: OrdBox Any -> OrdBox p
  coerceBox = unsafeCoerce

  coerceVal :: Any -> slot g i o
  coerceVal = unsafeCoerce

foreachSlot
  :: forall m ps slot
   . Applicative m
  => SlotStorage ps slot
  -> (forall g i o. slot g i o -> m Unit)
  -> m Unit
foreachSlot (SlotStorage m) k = traverse_ (k <<< coerceVal) m
  where
  coerceVal :: forall g i o. Any -> slot g i o
  coerceVal = unsafeCoerce
