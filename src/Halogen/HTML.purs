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
type ComponentHTML act ps m = HTML (ComponentSlot HTML ps m act) act

-- | A type useful for a chunk of HTML with no slot-embedding or query-raising.
-- |
-- | Often a polymorphic usage of `HTML` is good enough for this, but sometimes
-- | it's useful to have a type like this (and accompanying coercion) when doing
-- | things like creating components that accept a chunk of HTML as part of
-- | their configuration.
type PlainHTML = HTML Void Void

-- | Relaxes the type of `PlainHTML` to make it compatible with all `HTML`.
fromPlainHTML :: forall p i. PlainHTML -> HTML p i
fromPlainHTML = unsafeCoerce -- ≅ bimap absurd absurd

-- | Defines a slot for a child component. Takes:
-- | - the slot address label
-- | - the slot address index
-- | - the component for the slot
-- | - the input value to pass to the component
-- | - a function mapping outputs from the component to a query in the parent
slot
  :: forall sym px ps f i o p m act
   . Row.Cons sym (Slot f o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> Component HTML f i o m
  -> i
  -> (o -> Maybe act)
  -> ComponentHTML act ps m
slot sym p component input outputQuery =
  Core.slot (ComponentSlot (componentSlot sym p component input outputQuery))

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
  :: forall a act ps m
   . (a -> a -> Boolean)
  -> (a -> ComponentHTML act ps m)
  -> a
  -> ComponentHTML act ps m
memoized eqFn f a = Core.slot (ThunkSlot (thunked eqFn f a))

-- | Skips rendering for referentially equal arguments. You should not use this
-- | function fully saturated, but instead partially apply it for use within a
-- | Component's scope.
lazy
  :: forall a act ps m
   . (a -> ComponentHTML act ps m)
  -> a
  -> ComponentHTML act ps m
lazy f a = Core.slot (ThunkSlot (Fn.runFn2 thunk1 f a))

-- | Like `lazy`, but for a rendering function which takes 2 arguments.
lazy2
  :: forall a b act ps m
   . (a -> b -> ComponentHTML act ps m)
  -> a
  -> b
  -> ComponentHTML act ps m
lazy2 f a b = Core.slot (ThunkSlot (Fn.runFn3 thunk2 f a b))

-- | Like `lazy`, but for a rendering function which takes 3 arguments.
lazy3
  :: forall a b c act ps m
   . (a -> b -> c -> ComponentHTML act ps m)
  -> a
  -> b
  -> c
  -> ComponentHTML act ps m
lazy3 f a b c = Core.slot (ThunkSlot (Fn.runFn4 thunk3 f a b c))
