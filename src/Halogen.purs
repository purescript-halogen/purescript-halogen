-- | The base Halogen module re-exports most of the library's useful types and
-- | combinators, aside from the `HTML`-building functionality - the HTML
-- | modules export a large number of commonly named values that are likely to
-- | conflict.
module Halogen
  ( HalogenIO
  , HTML
  , IProp
  , module Data.Lazy
  , module Halogen.Component
  , module Halogen.HTML.Core
  , module Halogen.Query
  ) where

import Prelude

import Control.Coroutine as CR

import Data.Lazy (defer)

import Halogen.Component (Component, Component', ComponentDSL, ComponentHTML, ComponentSlot, ComponentSpec, LifecycleComponentSpec, ParentComponentSpec, ParentDSL, ParentHTML, ParentLifecycleComponentSpec, component, hoist, lifecycleComponent, lifecycleParentComponent, mkComponent, mkComponentSlot, parentComponent, unComponent, unComponentSlot)
import Halogen.HTML.Core (AttrName(..), ClassName(..), Namespace(..), PropName(..), ElemName(..))
import Halogen.HTML.Core as C
import Halogen.HTML.Properties as P
import Halogen.Query (Action, EventSource, HalogenF(..), HalogenM(..), RefLabel(..), Request, SubscribeStatus(..), action, checkSlot, eventSource, eventSource_, get, getHTMLElementRef, getRef, getSlots, gets, lift, liftAff, liftEff, mkQuery, modify, put, query, query', queryAll, queryAll', raise, request, subscribe)

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
