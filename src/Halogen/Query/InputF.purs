module Halogen.Query.InputF where

import Prelude

import Data.Bifunctor (class Bifunctor, rmap)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)

newtype RefLabel = RefLabel String

derive instance newtypeRefLabel :: Newtype RefLabel _
derive newtype instance eqRefLabel :: Eq RefLabel
derive newtype instance ordRefLabel :: Ord RefLabel

data InputF a i
  = RefUpdate RefLabel (Maybe Foreign) a
  | Query i

instance bifunctorInputF :: Bifunctor InputF where
  bimap f g = case _ of
    RefUpdate p mf a -> RefUpdate p mf (f a)
    Query i -> Query (g i)

instance functorInputF :: Functor (InputF a) where
  map = rmap
