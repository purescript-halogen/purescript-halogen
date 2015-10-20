-- | Functions and types used to describe the `HalogenF` algebra used in a
-- | component's `eval` and `peek` functions.
module Halogen.Query
  ( Action()
  , action
  , Request()
  , request
  , HalogenF()
  , get
  , gets
  , modify
  , subscribe
  , liftH
  , liftAff'
  , liftAff''
  , liftEff'
  , liftEff''
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

-- | Type synonym for an "action" - An action only causes effects and has no
-- | result value.
-- |
-- | In a query algebra, an action is any constructor that carries the algebra's
-- | type variable as a value. For example:
-- |
-- | ``` purescript
-- | data Query a
-- |   = SomeAction a
-- |   | SomeOtherAction String a
-- |   | NotAnAction (Boolean -> a)
-- | ```
-- |
-- | Both `SomeAction` and `SomeOtherAction` have `a` as a value so they are
-- | considered actions, whereas `NotAnAction` has `a` as the result of a
-- | function so is considered to be a "request" ([see below](#Request)).
type Action f = Unit -> f Unit

-- | Takes a data constructor of query algebra `f` and creates an action.
-- |
-- | For example:
-- |
-- | ```purescript
-- | data Query a = Tick a
-- |
-- | sendTick :: forall eff. Driver Query eff -> Aff (HalogenEffects eff) Unit
-- | sendTick driver = driver (action Tick)
-- | ```
action :: forall f. Action f -> f Unit
action act = act unit

-- | Type synonym for an "request" - a request can cause effects as well as
-- | fetching some information from a component.
-- |
-- | In a query algebra, an action is any constructor that carries the algebra's
-- | type variable as the return value of a function. For example:
-- |
-- | ``` purescript
-- | data Query a = SomeRequest (Boolean -> a)
-- | ```
type Request f a = forall i. (a -> i) -> f i

-- | Takes a data constructor of query algebra `f` and creates a request.
-- |
-- | For example:
-- |
-- | ```purescript
-- | data Query a = GetTickCount (Int -> a)
-- |
-- | getTickCount :: forall eff. Driver Query eff -> Aff (HalogenEffects eff) Int
-- | getTickCount driver = driver (request GetTickCount)
-- | ```
request :: forall f a. Request f a -> f a
request req = req id

-- | A type alias for the Halogen component algebra.
type HalogenF s f g = Coproduct (StateF s) (Coproduct (SubscribeF f g) g)

-- | Provides a way of accessing the current component's state within an `Eval`
-- | or `Peek` function. This is much like `get` for the `State` monad, but
-- | instead of operating in some `StateT`, uses the `HalogenF` algebra. For
-- | example:
-- |
-- | ``` purescript
-- | data Query a = GetState (State -> a)
-- |
-- | eval :: forall g. Eval Query State Query g
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
-- | data Query a = GetX (Number -> a)
-- | type State = { x :: Number, y :: Number }
-- |
-- | eval :: forall g. Eval Query State Query g
-- | eval (GetX k) = do
-- |   x <- gets _.x
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
-- | data Query a = Increment a
-- | type State = Int
-- |
-- | eval :: forall g. Eval Query State Query g
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
-- | without the need to use `liftH $ liftAff $ ...`.
liftAff' :: forall eff a s f g. (MonadAff eff g, Functor g) => Aff eff a -> Free (HalogenF s f g) a
liftAff' = liftH <<< liftAff

-- | A version of `liftAff'` that can be used in the `eval` or `peek` function
-- | of a parent component.
liftAff'' :: forall eff a s s' f f' g. (MonadAff eff g, Functor g) => Aff eff a -> Free (HalogenF s f (Free (HalogenF s' f' g))) a
liftAff'' = liftH <<< liftAff'

-- | A convenience function for lifting an `Eff` action directly into a
-- | `Free HalogenF` when there is a `MonadEff` instance for the current `g`,
-- | without the need to use `liftH $ liftEff $ ...`.
liftEff' :: forall eff a s f g. (MonadEff eff g, Functor g) => Eff eff a -> Free (HalogenF s f g) a
liftEff' = liftH <<< liftEff

-- | A version of `liftEff'` that can be used in the `eval` or `peek` function
-- | of a parent component.
liftEff'' :: forall eff a s s' f f' g. (MonadEff eff g, Functor g) => Eff eff a -> Free (HalogenF s f (Free (HalogenF s' f' g))) a
liftEff'' = liftH <<< liftEff'

-- | Changes the `g` for a `HalogenF`. Used internally by Halogen.
hoistHalogenF :: forall s f g h. (Functor h) => Natural g h -> Natural (HalogenF s f g) (HalogenF s f h)
hoistHalogenF nat = coproduct left (right <<< coproduct (left <<< hoistSubscribe nat) (right <<< nat))
