module Halogen.Query.ChildQuery where

import Prelude

import Data.Maybe (Maybe)
import Halogen.Data.Slot (SlotStorage)
import Unsafe.Coerce (unsafeCoerce)

data ChildQueryBox (ps :: # Type) a

data ChildQuery ps g o a f b =
  ChildQuery
    (forall slot m. Applicative m => (slot g o -> m (Maybe b)) -> SlotStorage ps slot -> m (f b))
    (g b)
    (f b -> a)

instance functorChildQuery :: Functor (ChildQueryBox ps) where
  map f = unChildQueryBox \(ChildQuery u q k) ->
    mkChildQueryBox (ChildQuery u q (f <<< k))

mkChildQueryBox
  :: forall ps g o a f b
   . ChildQuery ps g o a f b
  -> ChildQueryBox ps a
mkChildQueryBox = unsafeCoerce

unChildQueryBox
  :: forall ps a r
   . (forall g o f b. ChildQuery ps g o a f b -> r)
  -> ChildQueryBox ps a
  -> r
unChildQueryBox = unsafeCoerce
