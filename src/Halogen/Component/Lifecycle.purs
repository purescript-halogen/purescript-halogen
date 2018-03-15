module Halogen.Component.Lifecycle where

import Prelude

data Lifecycle a
  = Initialize
  | Receive a
  | Finalize

derive instance eqLifecycle :: Eq a => Eq (Lifecycle a)
derive instance ordLifecycle :: Ord a => Ord (Lifecycle a)
derive instance functorLifecycle :: Functor Lifecycle
