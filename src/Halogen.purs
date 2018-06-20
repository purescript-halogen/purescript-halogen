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

import Halogen.Component (Component, ComponentSlot, ComponentSpec, ComponentSpec', component, component', hoist, mkComponentSlot, unComponent, unComponentSlot)
import Halogen.Data.Slot (Slot)
import Halogen.HTML (ComponentHTML, ComponentHTML')
import Halogen.HTML.Core (AttrName(..), ClassName(..), Namespace(..), PropName(..), ElemName(..))
import Halogen.Query (Action, HalogenF(..), HalogenM, HalogenM'(..), HalogenQ(..), RefLabel(..), Request, SubscriptionId, action, fork, get, getHTMLElementRef, getRef, gets, lift, liftAff, liftEffect, modify, modify_, put, query, queryAll, raise, request, subscribe, subscribe', unsubscribe)

-- | A record produced when the root component in a Halogen UI has been run.
-- | `query` allows external sources to query the root component and `subscribe`
-- | allows external consumers to receive messages raised by the root component.
type HalogenIO f o m =
  { query :: f ~> m
  , subscribe :: CR.Consumer o m Unit -> m Unit
  }
