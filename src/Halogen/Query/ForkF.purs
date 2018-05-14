module Halogen.Query.ForkF where

import Prelude

import Effect.Aff (Aff)
import Effect.Exception (Error)

import Unsafe.Coerce (unsafeCoerce)

data ForkF f x a = ForkF (f x) ((Error -> Aff Unit) -> a)

fork :: forall f x. f x -> Fork f (Error -> Aff Unit)
fork fx = mkFork $ ForkF fx identity

data Fork (f :: Type -> Type) a

mkFork :: forall f x a. ForkF f x a -> Fork f a
mkFork = unsafeCoerce

unFork :: forall f a r. (forall x. ForkF f x a -> r) -> Fork f a -> r
unFork = unsafeCoerce

instance functorFork :: Functor (Fork f) where
  map f = unFork \(ForkF fx c) -> mkFork $ ForkF fx (map f c)

hoistFork :: forall f g a. (f ~> g) -> Fork f a -> Fork g a
hoistFork nat = unFork \(ForkF fx c) -> mkFork $ ForkF (nat fx) c
