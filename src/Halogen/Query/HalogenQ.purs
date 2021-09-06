module Halogen.Query.HalogenQ where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Coyoneda (Coyoneda)

data HalogenQ query action input a
  = Initialize a
  | Finalize a
  | Receive input a
  | Action action a
  | Query (Coyoneda query a) (Unit -> a)

instance bifunctorHalogenQ :: Bifunctor (HalogenQ query action) where
  bimap f g = case _ of
    Initialize a -> Initialize (g a)
    Finalize a -> Finalize (g a)
    Receive i a -> Receive (f i) (g a)
    Action action a -> Action action (g a)
    Query fa k -> Query (map g fa) (map g k)

derive instance functorHalogenQ :: Functor (HalogenQ query action input)
