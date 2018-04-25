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
import Data.Monoid (mempty)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..))
import Halogen.Data.OrdBox (OrdBox, mkOrdBox, unOrdBox)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Any :: Type

data Slot (g :: Type -> Type) o p

newtype SlotStorage (partitions :: # Type) (slot :: (Type -> Type) -> Type -> Type) =
  SlotStorage (Map (Tuple String (OrdBox Any)) Any)

empty :: forall ps slot. SlotStorage ps slot
empty = SlotStorage Map.empty

lookup
  :: forall sym px ps slot g o p
   . RowCons sym (Slot g o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> SlotStorage ps slot
  -> Maybe (slot g o)
lookup sym key (SlotStorage m) =
  coerceSlot (Map.lookup (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox key))) m)
  where
  coerceSlot :: Maybe Any -> Maybe (slot g o)
  coerceSlot = unsafeCoerce

  coerceBox :: OrdBox p -> OrdBox Any
  coerceBox = unsafeCoerce

pop
  :: forall sym px ps slot g o p
   . RowCons sym (Slot g o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> SlotStorage ps slot
  -> Maybe (Tuple (slot g o) (SlotStorage ps slot))
pop sym key (SlotStorage m) =
  coercePop (Map.pop (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox key))) m)
  where
  coercePop :: Maybe (Tuple Any (Map (Tuple String (OrdBox Any)) Any)) -> Maybe (Tuple (slot g o) (SlotStorage ps slot))
  coercePop = unsafeCoerce

  coerceBox :: OrdBox p -> OrdBox Any
  coerceBox = unsafeCoerce

insert
  :: forall sym px ps slot g o p
   . RowCons sym (Slot g o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> slot g o
  -> SlotStorage ps slot
  -> SlotStorage ps slot
insert sym key val (SlotStorage m) =
  SlotStorage (Map.insert (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox key))) (coerceVal val) m)
  where
  coerceBox :: OrdBox p -> OrdBox Any
  coerceBox = unsafeCoerce

  coerceVal :: slot g o -> Any
  coerceVal = unsafeCoerce

slots
  :: forall sym px ps slot g o p
   . RowCons sym (Slot g o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> SlotStorage ps slot
  -> Map p (slot g o)
slots sym (SlotStorage m) = Map.foldSubmap Nothing Nothing go m
  where
  key = reflectSymbol sym

  go (Tuple key' ob) val
    | key == key' = Map.singleton (unOrdBox (coerceBox ob)) (coerceVal val)
    | otherwise = mempty

  coerceBox :: OrdBox Any -> OrdBox p
  coerceBox = unsafeCoerce

  coerceVal :: Any -> slot g o
  coerceVal = unsafeCoerce

foreachSlot
  :: forall m ps slot
   . Applicative m
  => SlotStorage ps slot
  -> (forall g o. slot g o -> m Unit)
  -> m Unit
foreachSlot (SlotStorage m) k = traverse_ (k <<< coerceVal) m
  where
  coerceVal :: forall g o. Any -> slot g o
  coerceVal = unsafeCoerce
