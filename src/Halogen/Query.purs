-- | Functions and types used to describe the `HalogenF` algebra used in a
-- | component's `eval` function.
module Halogen.Query
  ( Action
  , action
  , Request
  , request
  , getHTMLElementRef
  , module Exports
  , module Halogen.Query.Input
  , module Halogen.Query.HalogenM
  , module Halogen.Query.HalogenQ
  ) where

import Prelude

import Control.Monad.State.Class (get, gets, modify, modify_, put) as Exports
import Control.Monad.Trans.Class (lift) as Exports
import Data.Maybe (Maybe)
import Effect.Aff.Class (liftAff) as Exports
import Effect.Class (liftEffect) as Exports
import Halogen.Query.HalogenM (HalogenM, HalogenM'(..), HalogenF(..), SubscriptionId, ForkId, fork, kill, getRef, query, queryAll, subscribe, subscribe', unsubscribe, raise)
import Halogen.Query.HalogenQ (HalogenQ(..))
import Halogen.Query.Input (RefLabel(..))
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement

-- | Type synonym for an "action-style" query - An action only causes effects
-- | and has no result value.
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
-- | sendTick :: forall o. HalogenIO Query o Aff -> Aff Unit
-- | sendTick app = app.query (action Tick)
-- | ```
action :: forall f. Action f -> f Unit
action act = act unit

-- | Type synonym for an "request-style" query - a request can cause effects as
-- | well as fetching some information from a component.
-- |
-- | In a query algebra, an action is any constructor that carries the algebra's
-- | type variable as the return value of a function. For example:
-- |
-- | ``` purescript
-- | data Query a = SomeRequest (Boolean -> a)
-- | ```
type Request f a = (a -> a) -> f a

-- | Takes a data constructor of query algebra `f` and creates a request.
-- |
-- | For example:
-- |
-- | ```purescript
-- | data Query a = GetTickCount (Int -> a)
-- |
-- | getTickCount :: forall o. HalogenIO Query o Aff -> Aff Int
-- | getTickCount app = app.query (request GetTickCount)
-- | ```
request :: forall f a. Request f a -> f a
request req = req identity

-- | Retrieves a `HTMLElement` value that is associated with a `Ref` in the
-- | rendered output of a component. If there is no currently rendered value (or
-- | it is not an `HTMLElement`) for the request will return `Nothing`.
getHTMLElementRef :: forall s f ps o m. RefLabel -> HalogenM s f ps o m (Maybe HTMLElement)
getHTMLElementRef = map (HTMLElement.fromElement =<< _) <<< getRef
