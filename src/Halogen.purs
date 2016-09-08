-- | The base Halogen module re-exports most of the library's useful types and
-- | combinators, aside from the `HTML`-building functionality - the HTML
-- | modules export a large number of commonly named values that are likely to
-- | conflict.
module Halogen
  ( HTML
  , Prop
  , module Data.Lazy
  , module Halogen.Component
  , module Halogen.Effects
  , module Halogen.HTML.Core
  , module Halogen.Query
  ) where

import Prelude

import Data.Lazy (defer)

import Halogen.Component (Component, Component', ComponentDSL, ComponentHTML, ComponentSlot, ComponentSpec, LifecycleComponentSpec, ParentComponentSpec, ParentDSL, ParentHTML, ParentLifecycleComponentSpec, component, getSlots, interpret, lifecycleComponent, lifecycleParentComponent, mkComponent, mkComponentSlot, parentComponent, query, query', queryAll, queryAll', transform, transformChild, unComponent, unComponentSlot)
import Halogen.Effects (HalogenEffects)
import Halogen.HTML.Core (AttrName, ClassName, EventName, Namespace, PropName, TagName, attrName, className, eventName, namespace, unAttrName, unClassName, unEventName, unNamespace, unPropName, unTagName, tagName)
import Halogen.HTML.Core as C
import Halogen.Query (Action, EventSource, ParentDSL, Request, HalogenF(..), StateF(..), action, eventSource, eventSource_, get, gets, liftAff, liftEff, lift, modify, raise, request, put, subscribe)

-- | A specialised version of the `Halogen.HTML.Core.HTML` type where `i` is
-- | `* -> *` kinded to match the kind of a component query algebra.
type HTML p i = C.HTML p (i Unit)

-- | A specialised version of the `Halogen.HTML.Core.Prop` type where `i` is
-- | `* -> *` kinded to match the kind of a component query algebra.
type Prop i = C.Prop (i Unit)
