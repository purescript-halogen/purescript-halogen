module Halogen.Query.ForkF where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error)

import Unsafe.Coerce (unsafeCoerce)

data ForkF eff f x a = ForkF (f x) ((Error -> Aff eff Boolean) -> a)

fork :: forall eff f x. f x -> Fork f (Error -> Aff eff Boolean)
fork fx = mkFork $ ForkF fx id

data Fork (f :: Type -> Type) a

mkFork :: forall eff f x a. ForkF eff f x a -> Fork f a
mkFork = unsafeCoerce

unFork :: forall f a r. (forall eff x. ForkF eff f x a -> r) -> Fork f a -> r
unFork = unsafeCoerce

instance functorFork :: Functor (Fork f) where
  map f = unFork \(ForkF fx c) -> mkFork $ ForkF fx (map f c)

hoistFork :: forall f g a. (f ~> g) -> Fork f a -> Fork g a
hoistFork nat = unFork \(ForkF fx c) -> mkFork $ ForkF (nat fx) c
