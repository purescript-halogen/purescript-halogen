module Halogen.Query.InputF where

import Prelude

import Data.Bifunctor (class Bifunctor, rmap)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)

data InputF p a i
  = RefUpdate p (Maybe Foreign) a
  | Query i

instance bifunctorInputF :: Bifunctor (InputF p) where
  bimap f g = case _ of
    RefUpdate p mf a -> RefUpdate p mf (f a)
    Query i -> Query (g i)

instance functorInputF :: Functor (InputF p a) where
  map = rmap
