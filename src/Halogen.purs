-- | The base Halogen module re-exports most of the library's useful types and
-- | combinators, aside from the `HTML`-building functionality - the HTML
-- | modules export a large number of commonly named values that are likely to
-- | conflict.
module Halogen
  ( HTML
  , Prop
  , module Halogen.Component.Slot
  , module Halogen.Component.Types
  , module Halogen.Driver
  , module Halogen.Effects
  , module Halogen.Query
  ) where

import Prelude

import Halogen.Component.Slot (ComponentSlot(..))
import Halogen.Component.Types (Component, Component', ComponentDSL, ComponentHTML, ComponentSpec, LifecycleComponentSpec, RenderResult, component, interpret, lifecycleComponent, mkComponent, transform, transformChild, unComponent)
import Halogen.Driver (Driver, runUI)
import Halogen.Effects (HalogenEffects)
import Halogen.HTML.Core as C
import Halogen.Query (Action, EventSource, ParentEventSource, Request, HalogenF(..), StateF(..), action, eventSource, eventSource_, fromAff, fromEff, get, gets, liftH, modify, request, set, subscribe, toParentEventSource)

-- | A specialised version of the `Halogen.HTML.Core.HTML` type where `i` is
-- | `* -> *` kinded to match the kind of a component query algebra.
type HTML p i = C.HTML p (i Unit)

-- | A specialised version of the `Halogen.HTML.Core.Prop` type where `i` is
-- | `* -> *` kinded to match the kind of a component query algebra.
type Prop i = C.Prop (i Unit)
