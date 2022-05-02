-- | Functions and types used to describe the `HalogenF` algebra used in a
-- | component's `eval` function.
module Halogen.Query
  ( Tell
  , mkTell
  , tell
  , tellAll
  , Request
  , mkRequest
  , request
  , requestAll
  , getHTMLElementRef
  , module Exports
  , module Halogen.Query.Input
  , module Halogen.Query.HalogenM
  , module Halogen.Query.HalogenQ
  ) where

import Prelude hiding (join)

import Control.Monad.State.Class (get, gets, modify, modify_, put) as Exports
import Control.Monad.Trans.Class (lift) as Exports
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Effect.Aff.Class (liftAff) as Exports
import Effect.Class (liftEffect) as Exports
import Halogen.Data.Slot (Slot)
import Halogen.Query.HalogenM (ForkId, HalogenF(..), HalogenM(..), SubscriptionId, fork, getRef, join, kill, query, queryAll, raise, subscribe, subscribe', unsubscribe)
import Halogen.Query.HalogenQ (HalogenQ(..))
import Halogen.Query.Input (RefLabel(..))
import Prim.Row as Row
import Type.Proxy (Proxy)
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
-- | sendTick app = app.query (H.mkTell Tick)
-- | ```
mkTell :: forall f. Tell f -> f Unit
mkTell act = act unit

tell
  :: forall state action output m label slots query output' slot _1
   . Row.Cons label (Slot query output' slot) _1 slots
  => IsSymbol label
  => Ord slot
  => Proxy label
  -> slot
  -> Tell query
  -> HalogenM state action slots output m Unit
tell slot label req = void $ query slot label (req unit)

tellAll
  :: forall state action output m label slots query output' slot _1
   . Row.Cons label (Slot query output' slot) _1 slots
  => IsSymbol label
  => Ord slot
  => Proxy label
  -> Tell query
  -> HalogenM state action slots output m Unit
tellAll label req = void $ queryAll label (req unit)

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
-- | getTickCount app = app.query (H.mkRequest GetTickCount)
-- | ```
mkRequest :: forall f a. Request f a -> f a
mkRequest req = req identity

request
  :: forall state action output m label slots query output' slot a _1
   . Row.Cons label (Slot query output' slot) _1 slots
  => IsSymbol label
  => Ord slot
  => Proxy label
  -> slot
  -> Request query a
  -> HalogenM state action slots output m (Maybe a)
request slot label req = query slot label (req identity)

requestAll
  :: forall state action output m label slots query output' slot a _1
   . Row.Cons label (Slot query output' slot) _1 slots
  => IsSymbol label
  => Ord slot
  => Proxy label
  -> Request query a
  -> HalogenM state action slots output m (Map slot a)
requestAll label req = queryAll label (req identity)

-- | Retrieves a `HTMLElement` value that is associated with a `Ref` in the
-- | rendered output of a component. If there is no currently rendered value (or
-- | it is not an `HTMLElement`) for the request will return `Nothing`.
getHTMLElementRef
  :: forall state action slots output m
   . RefLabel
  -> HalogenM state action slots output m (Maybe HTMLElement)
getHTMLElementRef = map (HTMLElement.fromElement =<< _) <<< getRef
