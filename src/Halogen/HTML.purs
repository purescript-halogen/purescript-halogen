-- | This module re-exports the types for the `HTML` DSL, and values for all
-- | supported HTML elements.
module Halogen.HTML
  ( ComponentHTML
  , PlainHTML
  , fromPlainHTML
  , slot
  , memoized
  , module Halogen.HTML.Core
  , module Halogen.HTML.Elements
  , module Halogen.HTML.Properties
  ) where

import Halogen.HTML.Elements

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy)
import Halogen.Component (Component, ComponentSlot(..), componentSlot)
import Halogen.Data.Slot (Slot)
import Halogen.HTML.Core (class IsProp, AttrName(..), ClassName(..), HTML(..), Namespace(..), PropName(..), ElemName(..), text, handler)
import Halogen.HTML.Core as Core
import Halogen.HTML.Properties (IProp, attr, attrNS, prop)
import Halogen.VDom.Thunk (thunk1, thunk2, thunk3, thunked)
import Prelude (class Ord, Void)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

-- | A convenience synonym for the output type of a `render` function, for a
-- | component that renders HTML.
type ComponentHTML action slots m = HTML (ComponentSlot HTML slots m action) action

-- | A type useful for a chunk of HTML with no slot-embedding or query-raising.
-- |
-- | Often a polymorphic usage of `HTML` is good enough for this, but sometimes
-- | it's useful to have a type like this (and accompanying coercion) when doing
-- | things like creating components that accept a chunk of HTML as part of
-- | their configuration.
type PlainHTML = HTML Void Void

-- | Relaxes the type of `PlainHTML` to make it compatible with all `HTML`.
fromPlainHTML :: forall w i. PlainHTML -> HTML w i
fromPlainHTML = unsafeCoerce -- ≅ bimap absurd absurd

-- | Defines a slot for a child component. Takes:
-- | - the slot address label
-- | - the slot address index
-- | - the component for the slot
-- | - the input value to pass to the component
-- | - a function mapping outputs from the component to a query in the parent
slot
  :: forall query action input output slots m label slot _1
   . Row.Cons label (Slot query output slot) _1 slots
  => IsSymbol label
  => Ord slot
  => SProxy label
  -> slot
  -> Component HTML query input output m
  -> input
  -> (output -> Maybe action)
  -> ComponentHTML action slots m
slot label p component input outputQuery =
  Core.widget (ComponentSlot (componentSlot label p component input outputQuery))

-- | Optimizes rendering of a subtree given an equality predicate. If an argument
-- | is deemed equivalent to the previous value, rendering and diffing will be
-- | skipped. You should not use this function fully saturated, but instead
-- | partially apply it for use within a Component's scope. For example, to skip
-- | rendering for equal states, just wrap your `render` function.
-- |
-- | ```purescript
-- | myComponent = component
-- |  { render: memoized eq render
-- |  , ...
-- |  }
-- | ```
memoized
  :: forall a action slots m
   . (a -> a -> Boolean)
  -> (a -> ComponentHTML action slots m)
  -> a
  -> ComponentHTML action slots m
memoized eqFn f a = Core.widget (ThunkSlot (thunked eqFn f a))

-- | Skips rendering for referentially equal arguments. You should not use this
-- | function fully saturated, but instead partially apply it for use within a
-- | Component's scope.
lazy
  :: forall a action slots m
   . (a -> ComponentHTML action slots m)
  -> a
  -> ComponentHTML action slots m
lazy f a = Core.widget (ThunkSlot (Fn.runFn2 thunk1 f a))

-- | Like `lazy`, but for a rendering function which takes 2 arguments.
lazy2
  :: forall a b action slots m
   . (a -> b -> ComponentHTML action slots m)
  -> a
  -> b
  -> ComponentHTML action slots m
lazy2 f a b = Core.widget (ThunkSlot (Fn.runFn3 thunk2 f a b))

-- | Like `lazy`, but for a rendering function which takes 3 arguments.
lazy3
  :: forall a b c action slots m
   . (a -> b -> c -> ComponentHTML action slots m)
  -> a
  -> b
  -> c
  -> ComponentHTML action slots m
lazy3 f a b c = Core.widget (ThunkSlot (Fn.runFn4 thunk3 f a b c))
