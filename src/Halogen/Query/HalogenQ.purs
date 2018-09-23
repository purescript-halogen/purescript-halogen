module Halogen.Query.HalogenQ where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Coyoneda (Coyoneda)

data HalogenQ f act i a
  = Initialize a
  | Finalize a
  | Receive i a
  | Handle act a
  | Request (Coyoneda f a) (Unit → a)

instance bifunctorHalogenQ :: Functor f => Bifunctor (HalogenQ f act) where
  bimap f g = case _ of
    Initialize a -> Initialize (g a)
    Finalize a -> Finalize (g a)
    Receive i a -> Receive (f i) (g a)
    Handle act a -> Handle act (g a)
    Request fa k -> Request (map g fa) (map g k)

derive instance functorHalogenQ :: Functor f => Functor (HalogenQ f act i)
