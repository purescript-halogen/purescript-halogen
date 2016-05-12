-- | A part of the `HalogenF` algebra that replicates a `MonadState`-like
-- | interface.
module Halogen.Query.StateF
  ( StateF(..)
  , mapState
  , stateN
  ) where

import Prelude

import Control.Monad.State as CMS

import Data.Functor (($>))
import Data.NaturalTransformation (Natural)

-- | The state algebra.
data StateF s a
  = Get (s -> a)
  | Modify (s -> s) a

instance functorStateF :: Functor (StateF s) where
  map f (Get k) = Get (f <<< k)
  map f (Modify g next) = Modify g (f next)

-- | Map over the state value using a function to extract the new state value
-- | from the old state, and a function for modifying the state.
mapState :: forall s t a. (t -> s) -> ((s -> s) -> t -> t) -> StateF s a -> StateF t a
mapState f _ (Get k) = Get (k <<< f)
mapState _ f (Modify g next) = Modify (f g) next

-- | A natural transformation for interpreting the state algebra as some
-- | `MonadState`-supporting monad. Used internally by Halogen.
stateN :: forall s m. (Monad m, CMS.MonadState s m) => Natural (StateF s) m
stateN (Get k) = CMS.get >>= pure <<< k
stateN (Modify f next) = CMS.modify f $> next
