-- | The base Halogen module re-exports most of the library's useful types and
-- | combinators, aside from the `HTML`-building functionality - the HTML
-- | modules export a large number of commonly named values that are likely to
-- | conflict.
module Halogen
  ( HTML
  , Prop
  , module Halogen.Component
  , module Halogen.Driver
  , module Halogen.Effects
  , module Halogen.Query
  ) where

import Prelude

import Halogen.Component (Component, ComponentDSL, ComponentHTML, ComponentSpec, LifecycleComponentSpec, LifecycleParentComponentSpec, ParentComponentSpec, ParentDSL, ParentHTML, ParentQuery, ParentState, QueryF, ChildF(..), SlotConstructor, childSlots, component, finalizeComponent, initializeComponent, interpret, lifecycleComponent, lifecycleParentComponent, liftQuery, mkQueries, mkQueries', mkQuery, mkQuery', parentComponent, parentState, query, query', queryAll, queryAll', queryComponent, renderComponent, runChildF, transform, transformChild)
import Halogen.Driver (Driver, runUI)
import Halogen.Effects (HalogenEffects)
import Halogen.Query (Action, EventSource, HalogenF, ParentEventSource, Request, HalogenFP(..), StateF(..), action, eventSource, eventSource_, fromAff, fromEff, get, gets, liftH, modify, request, set, subscribe, subscribe', toParentEventSource)
import Halogen.HTML.Core as C

-- | A specialised version of the `Halogen.HTML.Core.HTML` type where `i` is
-- | `* -> *` kinded to match the kind of a component query algebra.
type HTML p i = C.HTML p (i Unit)

-- | A specialised version of the `Halogen.HTML.Core.Prop` type where `i` is
-- | `* -> *` kinded to match the kind of a component query algebra.
type Prop i = C.Prop (i Unit)
