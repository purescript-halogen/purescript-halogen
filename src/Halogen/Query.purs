module Halogen.Query
  ( action
  , request
  , HalogenF()
  , get
  , gets
  , modify
  , subscribe
  , liftH
  , liftAff'
  , liftEff'
  , hoistHalogenF
  , module Halogen.Query.StateF
  , module Halogen.Query.SubscribeF
  ) where

import Prelude (Unit(), unit, id, Functor, (<<<))

import Control.Monad.Aff (Aff())
import Control.Monad.Aff.Class (MonadAff, liftAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (MonadEff, liftEff)
import Control.Monad.Free (Free(), liftF)

import Data.NaturalTransformation (Natural())
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)

import Halogen.Query.StateF (StateF(..))
import Halogen.Query.SubscribeF (SubscribeF(..), EventSource(), eventSource, eventSource_, hoistSubscribe)

-- | Takes a data constructor of query algebra `f` and creates an "action". An
-- | "action" only causes effects and has no result value.
-- |
-- | For example:
-- |
-- | ```purescript
-- | data Input a = Tick a
-- |
-- | sendTick :: forall eff. Driver Input eff -> Aff (HalogenEffects eff) Unit
-- | sendTick driver = driver (action Tick)
-- | ```
action :: forall f. (Unit -> f Unit) -> f Unit
action act = act unit

-- | Takes a data constructor of query algebra `f` and creates a "request". A
-- | "request" can cause effects as well as fetching some information from a
-- | component.
-- |
-- | For example:
-- |
-- | ```purescript
-- | data Input a = GetTickCount (Int -> a)
-- |
-- | getTickCount :: forall eff. Driver Input eff -> Aff (HalogenEffects eff) Int
-- | getTickCount driver = driver (request GetTickCount)
-- | ```
request :: forall f a. (forall i. (a -> i) -> f i) -> f a
request req = req id

-- | A type alias for the full Halogen component algebra.
type HalogenF s f g = Coproduct (StateF s) (Coproduct (SubscribeF f g) g)

-- | Provides a way of accessing the current component's state within an `Eval`
-- | or `Peek` function. This is much like `get` for the `State` monad, but
-- | instead of operating in some `StateT`, uses the `HalogenF` algebra. For
-- | example:
-- |
-- | ``` purescript
-- | data Input a = GetState (State -> a)
-- |
-- | eval :: forall g. (Functor g) => Eval Input (Free Input) State g
-- | eval (GetState k) = do
-- |   currentState <- get
-- |   pure (k currentState)
-- | ```
get :: forall s f g. Free (HalogenF s f g) s
get = gets id

-- | A version of [`get`](#get) that maps over the retrieved state before
-- | returning the result. Useful in cases where only a portion of the state is
-- | desired. For example:
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
gets :: forall s f g a. (s -> a) -> Free (HalogenF s f g) a
gets f = liftF (left (Get f))

-- | Provides a way of modifying the current component's state within an `Eval`
-- | or `Peek` function. This is much like `modify` for the `State` monad, but
-- | instead of operating in some `StateT`, uses the `HalogenF` algebra. For
-- | example:
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
modify :: forall s f g. (s -> s) -> Free (HalogenF s f g) Unit
modify f = liftF (left (Modify f unit))

-- | Provides a way of having a component subscribe to an `EventSource` from
-- | within an `Eval` or `Peek` function.
subscribe :: forall s f g. EventSource f g -> Free (HalogenF s f g) Unit
subscribe p = liftF (right (left (Subscribe p unit)))

-- | A convenience function for lifting a `g` value directly into
-- | `Free HalogenF` without the need to use `liftF $ right $ right $ ...`.
liftH :: forall a s f g. g a -> Free (HalogenF s f g) a
liftH = liftF <<< right <<< right

-- | A convenience function for lifting an `Aff` action directly into a
-- | `Free HalogenF` when there is a `MonadAff` instance for the current `g`,
-- | without the need to use `liftFI $ liftAff $ ...`.
liftAff' :: forall eff a s f g. (MonadAff eff g, Functor g) => Aff eff a -> Free (HalogenF s f g) a
liftAff' = liftH <<< liftAff

-- | A convenience function for lifting an `Eff` action directly into a
-- | `Free HalogenF` when there is a `MonadEff` instance for the current `g`,
-- | without the need to use `liftFI $ liftEff $ ...`.
liftEff' :: forall eff a s f g. (MonadEff eff g, Functor g) => Eff eff a -> Free (HalogenF s f g) a
liftEff' = liftH <<< liftEff

-- | Changes the underlying `g` monad for a `HalogenF` value.
hoistHalogenF :: forall s f g h. (Functor h) => Natural g h -> Natural (HalogenF s f g) (HalogenF s f h)
hoistHalogenF nat = coproduct left (right <<< coproduct (left <<< hoistSubscribe nat) (right <<< nat))
