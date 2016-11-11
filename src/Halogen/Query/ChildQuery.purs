module Halogen.Query.ChildQuery
  ( ChildQuery
  , childQuery
  , unChildQuery
  , hoistChildQuery
  ) where

import Prelude

import Data.Bifunctor (class Bifunctor)

import Unsafe.Coerce (unsafeCoerce)

data ChildQuery g m p a = ChildQuery p ((g ~> m) -> m a)

instance bifunctorChildQuery :: Functor m => Bifunctor (ChildQuery g m) where
  bimap f g (ChildQuery p k) = ChildQuery (f p) (map g <$> k)

instance functorChildQuery :: Functor m => Functor (ChildQuery g m p) where
  map f (ChildQuery p k) = ChildQuery p (map f <$> k)

childQuery :: forall g m p a. Applicative m => p -> g a -> ChildQuery g m p a
childQuery p q = ChildQuery p (_ $ q)

unChildQuery
  :: forall g m p a r
   . (p -> ((g ~> m) -> m a) -> r)
  -> ChildQuery g m p a
  -> r
unChildQuery f (ChildQuery p k) = f p k

hoistChildQuery
  :: forall g m m' p a
   . (m ~> m')
  -> ChildQuery g m p a
  -> ChildQuery g m' p a
hoistChildQuery _ =
  -- This is safe because it is impossible to construct a `ChildQuery` that has
  -- a continuation implemented as anything other than an application of the
  -- input `(g ~> m)` to a `g a` - this guarantees the `m` type is determined
  -- by the function that is passed in, and therefore we have no need
  -- to `imap` the continuation.
  unsafeCoerce
