module Halogen.Query.InputF where

import Prelude

import Data.Foreign (Foreign)
import Data.Maybe (Maybe)

data InputF p f a
  = RefUpdate p (Maybe Foreign) a
  | Query (f a)

derive instance functorInputF :: Functor f => Functor (InputF p f)
