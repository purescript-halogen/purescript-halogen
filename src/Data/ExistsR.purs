module Data.ExistsR where

import Unsafe.Coerce (unsafeCoerce)

-- | A variant of `Exists` that works for type constructors that accept a _row_
-- | of types.
data ExistsR (f :: # * -> *)

-- | The `mkExistsR` function is used to introduce a value of type `ExistsR f`,
-- | by providing a value of type `f a`, for some row of types `a` which will be
-- | hidden in the existentially-quantified type.
mkExistsR :: forall f a. f a -> ExistsR f
mkExistsR = unsafeCoerce

-- | The `runExistsR` function is used to eliminate a value of type `ExistsR f`.
-- | The rank 2 type ensures that the existentially-quantified type does not
-- | escape its scope. Since the function is required to work for _any_ row of
-- | types `a`, it will work for the existentially-quantified type.
runExistsR :: forall f r. (forall a. f a -> r) -> ExistsR f -> r
runExistsR = unsafeCoerce
