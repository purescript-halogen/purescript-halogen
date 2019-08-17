module Halogen.Query.Input where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Halogen.Surface (SurfaceElement)

newtype RefLabel = RefLabel String

derive instance newtypeRefLabel :: Newtype RefLabel _
derive newtype instance eqRefLabel :: Eq RefLabel
derive newtype instance ordRefLabel :: Ord RefLabel

data Input surface action
  = RefUpdate RefLabel (Maybe (SurfaceElement surface))
  | Action action

derive instance functorInput :: Functor (Input surface)
