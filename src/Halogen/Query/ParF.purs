module Halogen.Query.ParF where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

data ParF f x y a = ParF (x -> y -> a) (f x) (f y)
data Par (f :: * -> *) a

mkPar :: forall f x y a. ParF f x y a -> Par f a
mkPar = unsafeCoerce

unPar :: forall f a r. (forall x y. ParF f x y a -> r) -> Par f a -> r
unPar = unsafeCoerce

instance functorPar :: Functor (Par f) where
  map f = unPar \(ParF g fx fy) -> mkPar (ParF (\x y -> f (g x y)) fx fy)

hoistPar :: forall f f' a. (f ~> f') -> Par f a -> Par f' a
hoistPar nat = unPar \(ParF f fx fy) -> mkPar (ParF f (nat fx) (nat fy))
