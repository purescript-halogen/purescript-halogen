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

data Slot (f :: Type -> Type) o p

newtype SlotStorage (ps :: # Type) (slot :: (Type -> Type) -> Type -> Type) =
  SlotStorage (Map (Tuple String (OrdBox Any)) Any)

empty :: forall ps slot. SlotStorage ps slot
empty = SlotStorage Map.empty

lookup
  :: forall sym px ps slot f o p
   . Row.Cons sym (Slot f o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> SlotStorage ps slot
  -> Maybe (slot f o)
lookup sym key (SlotStorage m) =
  coerceSlot (Map.lookup (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox key))) m)
  where
  coerceSlot :: Maybe Any -> Maybe (slot f o)
  coerceSlot = unsafeCoerce

  coerceBox :: OrdBox p -> OrdBox Any
  coerceBox = unsafeCoerce

pop
  :: forall sym px ps slot f o p
   . Row.Cons sym (Slot f o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> SlotStorage ps slot
  -> Maybe (Tuple (slot f o) (SlotStorage ps slot))
pop sym key (SlotStorage m) =
  coercePop (Map.pop (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox key))) m)
  where
  coercePop :: Maybe (Tuple Any (Map (Tuple String (OrdBox Any)) Any)) -> Maybe (Tuple (slot f o) (SlotStorage ps slot))
  coercePop = unsafeCoerce

  coerceBox :: OrdBox p -> OrdBox Any
  coerceBox = unsafeCoerce

insert
  :: forall sym px ps slot f o p
   . Row.Cons sym (Slot f o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> slot f o
  -> SlotStorage ps slot
  -> SlotStorage ps slot
insert sym key val (SlotStorage m) =
  SlotStorage (Map.insert (Tuple (reflectSymbol sym) (coerceBox (mkOrdBox key))) (coerceVal val) m)
  where
  coerceBox :: OrdBox p -> OrdBox Any
  coerceBox = unsafeCoerce

  coerceVal :: slot f o -> Any
  coerceVal = unsafeCoerce

slots
  :: forall sym px ps slot f o p
   . Row.Cons sym (Slot f o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> SlotStorage ps slot
  -> Map p (slot f o)
slots sym (SlotStorage m) = Map.foldSubmap Nothing Nothing go m
  where
  key = reflectSymbol sym

  go (Tuple key' ob) val
    | key == key' = Map.singleton (unOrdBox (coerceBox ob)) (coerceVal val)
    | otherwise = mempty

  coerceBox :: OrdBox Any -> OrdBox p
  coerceBox = unsafeCoerce

  coerceVal :: Any -> slot f o
  coerceVal = unsafeCoerce

foreachSlot
  :: forall m ps slot
   . Applicative m
  => SlotStorage ps slot
  -> (forall f o. slot f o -> m Unit)
  -> m Unit
foreachSlot (SlotStorage m) k = traverse_ (k <<< coerceVal) m
  where
  coerceVal :: forall f o. Any -> slot f o
  coerceVal = unsafeCoerce
