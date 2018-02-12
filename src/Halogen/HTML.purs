-- | This module re-exports the types for the `HTML` DSL, and values for all
-- | supported HTML elements.
module Halogen.HTML
  ( PlainHTML
  , fromPlainHTML
  , slot
  , slot'
  , module Halogen.HTML.Core
  , module Halogen.HTML.Elements
  , module Halogen.HTML.Properties
  ) where

import Prelude (Unit, Void, (<<<))

import Data.Functor as F
import Data.Maybe (Maybe(..))
import Halogen.Component (Component, ParentHTML, mkComponentSlot, unComponent)
import Halogen.Component.ChildPath (ChildPath, injSlot, prjQuery, injQuery)
import Halogen.HTML.Core (class IsProp, AttrName(..), ClassName(..), HTML(..), Namespace(..), PropName(..), ElemName(..), text, handler)
import Halogen.HTML.Core as Core
import Halogen.HTML.Elements
import Halogen.HTML.Properties (IProp, attr, attrNS, prop)
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
  :: forall f m p i o g
   . p
  -> Component HTML g i o m
  -> i
  -> (o -> Maybe (f Unit))
  -> ParentHTML f g p m
slot p component input outputQuery =
  let f = unComponent _.receiver component
  in Core.slot (mkComponentSlot p component input f outputQuery Just)

-- | Defines a slot for a child component when a parent has multiple types of
-- | child component. Takes:
-- | - the `ChildPath` for this particular child component type
-- | - the slot "address" value
-- | - the component for the slot
-- | - the input value to pass to the component
-- | - a function mapping outputs from the component to a query in the parent
slot'
  :: forall f g g' p p' i o m
   . ChildPath g g' p p'
  -> p
  -> Component HTML g i o m
  -> i
  -> (o -> Maybe (f Unit))
  -> ParentHTML f g' p' m
slot' i p component input outputQuery =
  let
    pq = prjQuery i
    f = F.map (injQuery i) <<< unComponent _.receiver component
  in
    Core.slot (mkComponentSlot (injSlot i p) component input f outputQuery pq)
