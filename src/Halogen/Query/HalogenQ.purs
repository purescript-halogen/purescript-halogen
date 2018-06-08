module Halogen.Query.HalogenQ where

import Prelude

import Data.Bifunctor (class Bifunctor)

data HalogenQ f g i a
  = Initialize a
  | Finalize a
  | Receive i a
  | Internal (g a)
  | External (f a)

instance bifunctorHalogenQ :: (Functor f, Functor g) => Bifunctor (HalogenQ f g) where
  bimap f g = case _ of
    Initialize a -> Initialize (g a)
    Finalize a -> Finalize (g a)
    Receive i a -> Receive (f i) (g a)
    Internal ga -> Internal (map g ga)
    External fa -> External (map g fa)

derive instance functorHalogenQ :: (Functor f, Functor g) => Functor (HalogenQ f g i)
