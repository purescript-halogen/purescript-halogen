-- | This module re-exports the types for the `HTML` DSL, and values for all
-- | supported HTML elements.
module Halogen.HTML
  ( PlainHTML
  , fromPlainHTML
  , slot
  , module Halogen.HTML.Core
  , module Halogen.HTML.Elements
  , module Halogen.HTML.Properties
  ) where

import Halogen.HTML.Elements

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol, SProxy)
import Halogen.Component (Component, ParentHTML, mkComponentSlot)
import Halogen.Data.Slot (Slot)
import Halogen.HTML.Core (class IsProp, AttrName(..), ClassName(..), HTML(..), Namespace(..), PropName(..), ElemName(..), text, handler)
import Halogen.HTML.Core as Core
import Halogen.HTML.Properties (IProp, attr, attrNS, prop)
import Prelude (class Ord, Unit, Void)
import Unsafe.Coerce (unsafeCoerce)

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
-- | - the slot "address" value
-- | - the component for the slot
-- | - the input value to pass to the component
-- | - a function mapping outputs from the component to a query in the parent
slot
  :: forall sym px ps g i o p m f
   . RowCons sym (Slot g o p) px ps
  => IsSymbol sym
  => Ord p
  => SProxy sym
  -> p
  -> Component HTML g i o m
  -> i
  -> (o -> Maybe (f Unit))
  -> ParentHTML f ps m
slot sym p component input outputQuery =
  Core.slot (mkComponentSlot sym p component input outputQuery)
