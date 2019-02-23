module Halogen.Query.HalogenQ where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Coyoneda (Coyoneda)

data HalogenQ f act i a
  = Initialize a
  | Finalize a
  | Receive i a
  | Action act a
  | Query (Coyoneda f a) (Unit → a)

instance bifunctorHalogenQ :: Functor f => Bifunctor (HalogenQ f act) where
  bimap f g = case _ of
    Initialize a -> Initialize (g a)
    Finalize a -> Finalize (g a)
    Receive i a -> Receive (f i) (g a)
    Action act a -> Action act (g a)
    Query fa k -> Query (map g fa) (map g k)

derive instance functorHalogenQ :: Functor f => Functor (HalogenQ f act i)
