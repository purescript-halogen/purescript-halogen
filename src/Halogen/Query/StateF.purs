-- | A part of the `HalogenF` algebra that replicates a `MonadState`-like
-- | interface, used to represent state changes in a component in the result of
-- | a component’s `eval` function.
module Halogen.Query.StateF
  ( StateF(..)
  , get
  , gets
  , modify
  , stateN
  ) where

import Prelude

import Control.Monad.Free (Free(), liftFI)
import qualified Control.Monad.State as CMS
import qualified Control.Monad.State.Class as CMS

import Data.Functor (($>))
import Data.Inject (Inject)
import Data.NaturalTransformation (Natural())

-- | The state algebra.
data StateF s a
  = Get (s -> a)
  | Modify (s -> s) a

instance functorStateF :: Functor (StateF s) where
  map f (Get k) = Get (f <<< k)
  map f (Modify g next) = Modify g (f next)

-- | Injects a `Get` action into a `Free` monad that is making use of the state
-- | algebra.
-- |
-- | This allows `get` to be used in a similar way to the version for the
-- | `State` monad when operating in the `eval` function for a component.
-- | For example:
-- |
-- | ``` purescript
-- | data Input a = GetState (State -> a)
-- |
-- | eval :: forall g. (Functor g) => Eval Input (Free Input) State g
-- | eval (GetState k) = do
-- |   currentState <- get
-- |   pure (k currentState)
-- | ```
get :: forall f s. (Inject (StateF s) f) => Free f s
get = gets id

-- | A version of `get` that maps over the retrieved state before returning the
-- | result. Useful in cases where only a portion of the state is desired, for
-- | example:
-- |
-- | ``` purescript
-- | data Input a = GetX (Number -> a)
-- | newtype State = State { x :: Number, y :: Number }
-- |
-- | eval :: forall g. (Functor g) => Eval Input (Free Input) State g
-- | eval (GetX k) = do
-- |   x <- gets \(State st) -> st.x
-- |   pure (k x)
-- | ```
gets :: forall f s a. (Inject (StateF s) f) => (s -> a) -> Free f a
gets f = liftFI (Get f)

-- | Injects a `Modify` action into a `Free` monad that is making use of the
-- | state algebra.
-- |
-- | This allows `modify` to be used in a similar way to the version for the
-- | `State` monad when operating in the `eval` function for a component.
-- | For example:
-- |
-- | ``` purescript
-- | data Input a = Increment a
-- | type State = Int
-- |
-- | eval :: Eval Input (Free Input) State g
-- | eval (Increment next) = do
-- |   modify (+ 1)
-- |   pure next
-- | ```
modify :: forall f s. (Inject (StateF s) f) => (s -> s) -> Free f Unit
modify f = liftFI (Modify f unit)

-- | A natural transformation for interpreting the state algebra as some
-- | `MonadState`-supporting monad. Used internally by Halogen in `runUI`.
stateN :: forall s m. (Monad m, CMS.MonadState s m) => Natural (StateF s) m
stateN (Get k) = CMS.get >>= pure <<< k
stateN (Modify f next) = CMS.modify f $> next
