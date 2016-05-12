-- | Functions and types used to describe the `HalogenF` algebra used in a
-- | component's `eval` and `peek` functions.
module Halogen.Query
  ( Action
  , action
  , Request
  , request
  , get
  , gets
  , modify
  , set
  , subscribe
  , subscribe'
  , liftH
  , module Control.Monad.Aff.Free
  , module Halogen.Query.EventSource
  , module Halogen.Query.HalogenF
  , module Halogen.Query.StateF
  ) where

import Prelude

import Control.Monad.Aff.Free (fromAff, fromEff)
import Control.Monad.Free (Free, liftF)

import Halogen.Query.EventSource (EventSource, ParentEventSource, eventSource, eventSource_, toParentEventSource)
import Halogen.Query.HalogenF (HalogenF, HalogenFP(..))
import Halogen.Query.StateF (StateF(..))

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
get :: forall e s f g. Free (HalogenFP e s f g) s
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
gets :: forall e s f g a. (s -> a) -> Free (HalogenFP e s f g) a
gets = liftF <<< StateHF <<< Get

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
modify :: forall e s f g. (s -> s) -> Free (HalogenFP e s f g) Unit
modify f = liftF (StateHF (Modify f unit))

-- | Provides a way of replacing the current component's state within an `Eval`
-- | or `Peek` function. This is much like `set` for the `State` monad, but
-- | instead of operating in some `StateT`, uses the `HalogenF` algebra.
set :: forall e s f g. s -> Free (HalogenFP e s f g) Unit
set = modify <<< const

-- | Provides a way of having a component subscribe to an `EventSource` from
-- | within an `Eval` function.
subscribe :: forall s f g. EventSource f g -> Free (HalogenFP EventSource s f g) Unit
subscribe es = liftF (SubscribeHF es unit)

-- | Provides a way of having a parent component subscribe to an `EventSource`
-- | from within an `Eval` or `Peek` function.
subscribe' :: forall s s' f f' g. EventSource f g -> Free (HalogenFP ParentEventSource s f (Free (HalogenFP EventSource s' f' g))) Unit
subscribe' es = liftF (SubscribeHF (toParentEventSource es) unit)

-- | A convenience function for lifting a `g` value directly into
-- | `Free HalogenF` without the need to use `liftF $ right $ right $ ...`.
liftH :: forall a e s f g. g a -> Free (HalogenFP e s f g) a
liftH = liftF <<< QueryHF
