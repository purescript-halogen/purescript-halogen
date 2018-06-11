module Halogen.Query.InputF where

import Prelude

import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Web.DOM (Element)

newtype RefLabel = RefLabel String

derive instance newtypeRefLabel :: Newtype RefLabel _
derive newtype instance eqRefLabel :: Eq RefLabel
derive newtype instance ordRefLabel :: Ord RefLabel

data InputF act
  = RefUpdate RefLabel (Maybe Element)
  | Query act

derive instance functorInputF :: Functor InputF
