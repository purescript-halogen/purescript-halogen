module Halogen.Query.ForkF
  -- ( Fork
  -- , mkFork
  -- , unFork
  -- , hoistFork
  -- )
  where

import Prelude

import Data.Coyoneda
import Data.Profunctor
import Data.Functor.Contravariant


import Control.Monad.Fork (Canceler, hoistCanceler)

import Unsafe.Coerce (unsafeCoerce)

data ForkF f x a = ForkF (f x) (Canceler f -> a)
data Fork (f :: * -> *) a

mkFork :: forall f x. f x → Fork f (Canceler f)
mkFork fx = mkFork' $ ForkF fx id

mkFork' :: forall f x a. ForkF f x a → Fork f a
mkFork' = unsafeCoerce

unFork :: forall f a r. (forall x. ForkF f x a → r) → Fork f a → r
unFork = unsafeCoerce

-- instance functorFork :: Functor (Fork f) where
--   map f = unFork \(ForkF fx c) -> mkFork (ForkF fx (map f c))

hoistFork :: forall f f' a. (f ~> f') -> Fork f a -> Fork f' a
hoistFork nat = unsafeCoerce --
  -- unFork \(ForkF fx c a) ->
  --   mkFork' (ForkF (nat fx) (hoistCanceler nat c) a)

hoy :: forall f g h a b. (Contravariant f, Functor g) => (g ~> h) -> (f (g a) -> b) -> f (h a) -> b
hoy nat = lmap (cmap nat)
