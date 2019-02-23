-- | Functions and types used to describe the `HalogenF` algebra used in a
-- | component's `eval` function.
module Halogen.Query
  ( Tell
  , tell
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
import Halogen.Query.HalogenM (HalogenM(..), HalogenF(..), SubscriptionId, ForkId, fork, kill, getRef, query, queryAll, subscribe, subscribe', unsubscribe, raise)
import Halogen.Query.HalogenQ (HalogenQ(..))
import Halogen.Query.Input (RefLabel(..))
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement

-- | Type synonym for a "tell-style" query - queries that only cause effects,
-- | but that cannot receive a return value.
-- |
-- | In a query algebra, a tell constructor carries the algebra's type variable
-- | as its last argument. For example:
-- |
-- | ``` purescript
-- | data Query a
-- |   = SomeTell a
-- |   | SomeOtherTell String a
-- |   | NotATell (Boolean -> a)
-- | ```
-- |
-- | Both `SomeTell` and `SomeOtherTell` carry a plain `a` as a value, whereas
-- | `NotATell` has `a` as the result of a function so is considered to be a
-- | "request" ([see below](#Request)).
type Tell f = Unit -> f Unit

-- | Takes a data constructor of query algebra `f` and creates a tell query.
-- |
-- | For example:
-- |
-- | ```purescript
-- | data Query a = Tick a
-- |
-- | sendTick :: forall o. H.HalogenIO Query o Aff -> Aff (Maybe Unit)
-- | sendTick app = app.query (H.tell Tick)
-- | ```
tell :: forall f. Tell f -> f Unit
tell act = act unit

-- | Type synonym for an "request-style" query - queries that can cause effects
-- | as well as fetching some information from a component.
-- |
-- | In a query algebra, a request constructor carries the algebra's type
-- | variable as the return value of a function as its last argument. For
-- | example:
-- |
-- | ``` purescript
-- | data Query a = SomeRequest (Boolean -> a)
-- | ```
type Request f a = (a -> a) -> f a

-- | Takes a data constructor of query algebra `f` and creates a request query.
-- |
-- | For example:
-- |
-- | ```purescript
-- | data Query a = GetTickCount (Int -> a)
-- |
-- | getTickCount :: forall o. H.HalogenIO Query o Aff -> Aff (Maybe Int)
-- | getTickCount app = app.query (H.request GetTickCount)
-- | ```
request :: forall f a. Request f a -> f a
request req = req identity

-- | Retrieves a `HTMLElement` value that is associated with a `Ref` in the
-- | rendered output of a component. If there is no currently rendered value (or
-- | it is not an `HTMLElement`) for the request will return `Nothing`.
getHTMLElementRef
  :: forall surface action slots output m
   . RefLabel
  -> HalogenM surface action slots output m (Maybe HTMLElement)
getHTMLElementRef = map (HTMLElement.fromElement =<< _) <<< getRef
