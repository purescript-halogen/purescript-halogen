-- | The base Halogen module re-exports most of the library's useful types and
-- | combinators, aside from the `HTML`-building functionality - the HTML
-- | modules export a large number of commonly named values that are likely to
-- | conflict.
module Halogen
  ( HalogenIO
  , HTML
  , IProp
  , module Data.Lazy
  , module Halogen.Data.Slot
  , module Halogen.Component
  , module Halogen.HTML.Core
  , module Halogen.Query
  ) where

import Prelude

import Control.Coroutine as CR

import Data.Lazy (defer)

import Halogen.Component (Component, ComponentHTML, ComponentHTML', ComponentSlot, ComponentSpec, ComponentSpec', component, component', hoist, mkComponentSlot, unComponent, unComponentSlot)
import Halogen.Data.Slot (Slot)
import Halogen.HTML.Core (AttrName(..), ClassName(..), Namespace(..), PropName(..), ElemName(..))
import Halogen.HTML.Core as C
import Halogen.HTML.Properties as P
import Halogen.Query (Action, HalogenF(..), HalogenM, HalogenM'(..), HalogenQ(..), RefLabel(..), Request, SubscriptionId, action, fork, get, getHTMLElementRef, getRef, gets, lift, liftAff, liftEffect, modify, modify_, put, query, queryAll, raise, request, subscribe, subscribe', unsubscribe)

-- | A record produced when the root component in a Halogen UI has been run.
-- | `query` allows external sources to query the root component and `subscribe`
-- | allows external consumers to receive messages raised by the root component.
type HalogenIO f o m =
  { query :: f ~> m
  , subscribe :: CR.Consumer o m Unit -> m Unit
  }

-- | A specialised version of the `Halogen.HTML.Core.HTML` type where `i` is
-- | `* -> *` kinded to match the kind of a component query algebra.
type HTML p i = C.HTML p (i Unit)

-- | A specialised version of the `Halogen.HTML.Properties.IProp` type where
-- | `i` is `* -> *` kinded to match the kind of a component query algebra.
type IProp r i = P.IProp r (i Unit)
