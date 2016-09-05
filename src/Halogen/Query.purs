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
  , put
  , subscribe
  , raise
  , module Control.Monad.Aff.Class
  , module Control.Monad.Eff.Class
  , module Control.Monad.Trans
  , module Halogen.Component
  , module Halogen.Query.EventSource
  , module Halogen.Query.HalogenF
  , module Halogen.Query.StateF
  )
  where

import Prelude

import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Trans (lift)
import Control.Monad.Free (liftF)

import Halogen.Component (ParentDSL, query, queryAll)
import Halogen.Query.EventSource (EventSource, eventSource, eventSource_)
import Halogen.Query.HalogenF (HalogenF(..))
import Halogen.Query.HalogenM (HalogenM(..))
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
get :: forall s f g p o m. HalogenM s f g p o m s
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
gets :: forall s f g p o m a. (s -> a) -> HalogenM s f g p o m a
gets = HalogenM <<< liftF <<< State <<< Get

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
modify :: forall s f g p o m. (s -> s) -> HalogenM s f g p o m Unit
modify f = HalogenM (liftF (State (Modify f unit)))

-- | Provides a way of replacing the current component's state within an `Eval`
-- | or `Peek` function. This is much like `put` for the `State` monad, but
-- | instead of operating in some `StateT`, uses the `HalogenF` algebra.
put :: forall s f g p o m. s -> HalogenM s f g p o m Unit
put = modify <<< const

-- | Provides a way of having a component subscribe to an `EventSource` from
-- | within an `Eval` function.
subscribe :: forall s f g p o m. EventSource f m -> HalogenM s f g p o m Unit
subscribe es = HalogenM (liftF (Subscribe es unit))

raise :: forall s f g p o m. o -> HalogenM s f g p o m Unit
raise o = HalogenM (liftF (Raise o unit))
