-- | This module re-exports the types for the `HTML` DSL, and values for all
-- | supported HTML elements.
module Halogen.HTML
  ( ComponentHTML'
  , ComponentHTML
  , PlainHTML
  , fromPlainHTML
  , slot
  , module Halogen.HTML.Core
  , module Halogen.HTML.Elements
  , module Halogen.HTML.Properties
  ) where

import Halogen.HTML.Elements

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy)
import Halogen.Component (Component, ComponentSlot, componentSlot)
import Halogen.Data.Slot (Slot)
import Halogen.HTML.Core (class IsProp, AttrName(..), ClassName(..), HTML(..), Namespace(..), PropName(..), ElemName(..), text, handler)
import Halogen.HTML.Core as Core
import Halogen.HTML.Properties (IProp, attr, attrNS, prop)
import Prelude (class Ord, Unit, Void)
import Prim.Row as Row
import Unsafe.Coerce (unsafeCoerce)

type ComponentHTML' act ps m = HTML (ComponentSlot HTML ps m act) act

-- | A convenience synonym for the output type of a `render` function, for a
-- | component that renders HTML.
type ComponentHTML f ps m = ComponentHTML' (f Unit) ps m

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
  -> ComponentHTML' act ps m
slot sym p component input outputQuery =
  Core.slot (componentSlot sym p component input outputQuery)
