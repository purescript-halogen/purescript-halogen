module Halogen.Query.HalogenQ where

import Prelude

import Data.Bifunctor (class Bifunctor)

data HalogenQ f i a
  = Initialize a
  | Finalize a
  | Receive i a
  | Internal (f a)

instance bifunctorHalogenQ :: Functor f => Bifunctor (HalogenQ f) where
  bimap f g = case _ of
    Initialize a -> Initialize (g a)
    Finalize a -> Finalize (g a)
    Receive i a -> Receive (f i) (g a)
    Internal fa -> Internal (map g fa)

derive instance functorHalogenQ :: Functor f => Functor (HalogenQ f i)
