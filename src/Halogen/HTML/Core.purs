module Halogen.HTML.Core
  ( HTML(..)
  , slot
  , text
  , element
  , keyed
  , prop
  , attr
  , handler
  , class IsProp
  , toPropValue
  , PropName(..)
  , AttrName(..)
  , ClassName(..)
  , module Exports
  ) where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap, rmap)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)

import DOM.Event.Types (Event, EventType)

import Halogen.VDom as VDom
import Halogen.VDom.DOM.Prop (Prop(..), PropValue, propFromBoolean, propFromInt, propFromNumber, propFromString)

import Unsafe.Coerce (unsafeCoerce)

import Halogen.VDom (ElemName(..), Namespace(..)) as Exports
import Halogen.VDom.DOM.Prop (Prop(..), ElemRef(..), PropValue) as Exports

newtype HTML p i = HTML (VDom.VDom (Array (Prop i)) p)

derive instance newtypeHTML :: Newtype (HTML p i) _

instance bifunctorHTML :: Bifunctor HTML where
  bimap f g (HTML vdom) = HTML (bimap (map (map g)) f vdom)

instance functorHTML :: Functor (HTML p) where
  map = rmap

-- | A smart constructor for widget slots in the HTML.
slot :: forall p q. p -> HTML p q
slot = HTML <<< VDom.Widget

-- | Constructs a text node `HTML` value.
text :: forall p i. String -> HTML p i
text = HTML <<< VDom.Text

-- | A smart constructor for HTML elements.
element :: forall p i. VDom.ElemName -> Array (Prop i) -> Array (HTML p i) -> HTML p i
element = coe (\name props children -> VDom.Elem (VDom.ElemSpec Nothing name props) children)
  where
  coe
    :: (VDom.ElemName -> Array (Prop i) -> Array (VDom.VDom (Array (Prop i)) p) -> VDom.VDom (Array (Prop i)) p)
    -> VDom.ElemName -> Array (Prop i) -> Array (HTML p i) -> HTML p i
  coe = unsafeCoerce

-- | A smart constructor for HTML elements with keyed children.
keyed :: forall p i. VDom.ElemName -> Array (Prop i) -> Array (Tuple String (HTML p i)) -> HTML p i
keyed = coe (\name props children -> VDom.Keyed (VDom.ElemSpec Nothing name props) children)
  where
  coe
    :: (VDom.ElemName -> Array (Prop i) -> Array (Tuple String (VDom.VDom (Array (Prop i)) p)) -> VDom.VDom (Array (Prop i)) p)
    -> VDom.ElemName -> Array (Prop i) -> Array (Tuple String (HTML p i)) -> HTML p i
  coe = unsafeCoerce

-- | Create a HTML property.
prop :: forall value i. IsProp value => PropName value -> Maybe AttrName -> value -> Prop i
prop (PropName name) an v = Property name (toPropValue v)

-- | Create a HTML attribute.
attr :: forall i. AttrName -> String -> Prop i
attr (AttrName name) = Attribute Nothing name

-- | Create an event handler.
handler :: forall i. EventType -> (Event -> Maybe i) -> Prop i
handler = Handler

class IsProp a where
  toPropValue :: a -> PropValue

instance stringIsProp :: IsProp String where
  toPropValue = propFromString

instance intIsProp :: IsProp Int where
  toPropValue = propFromInt

instance numberIsProp :: IsProp Number where
  toPropValue = propFromNumber

instance booleanIsProp :: IsProp Boolean where
  toPropValue = propFromBoolean

-- | A type-safe wrapper for property names.
-- |
-- | The phantom type `value` describes the type of value which this property
-- | requires.
newtype PropName value = PropName String

derive instance newtypePropName :: Newtype (PropName value) _
derive newtype instance eqPropName :: Eq (PropName value)
derive newtype instance ordPropName :: Ord (PropName value)
derive instance genericPropName :: Generic (PropName value)

-- | A type-safe wrapper for attribute names.
newtype AttrName = AttrName String

derive instance newtypeAttrName :: Newtype AttrName _
derive newtype instance eqAttrName :: Eq AttrName
derive newtype instance ordAttrName :: Ord AttrName
derive instance genericAttrName :: Generic AttrName

-- | A wrapper for strings which are used as CSS classes.
newtype ClassName = ClassName String

derive instance newtypeClassName :: Newtype ClassName _
derive newtype instance eqClassName :: Eq ClassName
derive newtype instance ordClassName :: Ord ClassName
derive instance genericClassName :: Generic ClassName
