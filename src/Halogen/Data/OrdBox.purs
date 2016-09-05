module Halogen.Data.OrdBox
  ( OrdBox
  , mkOrdBox
  , unOrdBox
  ) where

import Prelude

-- | A value carrying its `Ord` instance so it can be used at a later date
-- | without the need for evidence of the instance.
data OrdBox a = OrdBox (a -> a -> Boolean) (a -> a -> Ordering) a

instance eqOrdBox :: Eq (OrdBox a) where
  eq (OrdBox e _ x) (OrdBox _ _ y) = e x y

instance ordOrdBox :: Ord (OrdBox a) where
  compare (OrdBox _ c x) (OrdBox _ _ y) = c x y

mkOrdBox :: forall a. Ord a => a -> OrdBox a
mkOrdBox = OrdBox eq compare

unOrdBox :: forall a. OrdBox a -> a
unOrdBox (OrdBox _ _ a) = a
