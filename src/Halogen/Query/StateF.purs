-- | A part of the `HalogenF` algebra that replicates a `MonadState`-like
-- | interface.
module Halogen.Query.StateF where

import Prelude

-- | The state algebra.
data StateF s a
  = Get (s -> a)
  | Modify (s -> s) a

instance functorStateF :: Functor (StateF s) where
  map f (Get k) = Get (f <<< k)
  map f (Modify g next) = Modify g (f next)
