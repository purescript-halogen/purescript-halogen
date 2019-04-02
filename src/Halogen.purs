-- | The base Halogen module re-exports most of the library's useful types and
-- | combinators, aside from the `HTML`-building functionality - the HTML
-- | modules export a large number of commonly named values that are likely to
-- | conflict.
module Halogen
  ( HalogenIO
  , module Data.Lazy
  , module Halogen.Data.Slot
  , module Halogen.Component
  , module Halogen.HTML
  , module Halogen.HTML.Core
  , module Halogen.Query
  ) where

import Prelude

import Control.Coroutine as CR
import Data.Lazy (defer)
import Data.Maybe (Maybe)
import Halogen.Component (Component, ComponentSpec, ComponentSlot, ComponentSlotSpec, mkEval, defaultEval, mkComponent, hoist, componentSlot, unComponent, unComponentSlot)
import Halogen.Data.Slot (Slot)
import Halogen.HTML (ComponentHTML)
import Halogen.HTML.Core (AttrName(..), ClassName(..), Namespace(..), PropName(..), ElemName(..))
import Halogen.Query (ForkId, HalogenF(..), HalogenM(..), HalogenQ(..), RefLabel(..), Request, SubscriptionId, Tell, fork, get, getHTMLElementRef, getRef, gets, kill, lift, liftAff, liftEffect, modify, modify_, put, query, queryAll, raise, request, subscribe, subscribe', tell, unsubscribe)

-- | A record produced when the root component in a Halogen UI has been run.
-- |
-- | - `query` allows external sources to query the root component
-- | - `subscribe` allows external consumers to receive messages raised by the
-- |   root component
-- | - `dispose` stops running the UI and finalizes the root component
type HalogenIO query output m =
  { query :: forall a. query a -> m (Maybe a)
  , subscribe :: CR.Consumer output m Unit -> m Unit
  , dispose :: m Unit
  }
