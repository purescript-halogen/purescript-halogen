--| The core types and smart constructors for the HTML DSL.
module Halogen.HTML.Core
  ( Fuse
  , lowerFuse

  , HTML(..)
  , element
  , fillSlot

  , Prop(..)
  , PropF(..)
  , prop
  , attr
  , handler

  , class IsProp
  , toPropString

  , Namespace(..)
  , TagName(..)
  , PropName(..)
  , AttrName(..)
  , ClassName(..)
  ) where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap, rmap)
import Data.Exists (Exists, mkExists)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

import DOM.Event.Types (Event, EventType)
import DOM.HTML.Types (HTMLElement)

import Unsafe.Coerce (unsafeCoerce)

-- | An initial encoding of HTML nodes.
data HTML p i
  = Text String
  | Element (Maybe Namespace) TagName (Array (Prop i)) (Array (HTML p i))
  | Slot p
  | Fuse (Fuse p i)

instance bifunctorHTML :: Bifunctor HTML where
  bimap f g = case _ of
    Fuse bc -> Fuse (bimap f g bc)
    x -> Fuse (fuse f g x)

instance functorHTML :: Functor (HTML p) where
  map = rmap

-- | A smart constructor for HTML elements.
element :: forall p i. TagName -> Array (Prop i) -> Array (HTML p i) -> HTML p i
element = Element Nothing

-- | Populates the slot placeholder values in a `HTML` value.
fillSlot :: forall p p' i i' m. Applicative m => (p -> m (HTML p' i')) -> (i -> i') -> HTML p i -> m (HTML p' i')
fillSlot _ _ (Text s) = pure $ Text s
fillSlot f g (Element ns name props els) = Element ns name (map g <$> props) <$> traverse (fillSlot f g) els
fillSlot f _ (Slot p) = f p
fillSlot f g (Fuse h) = fillSlot f g (lowerFuse h)

-- | A property can be:
-- | - A JavaScript property for an element (typed, and may not have a
-- |   corresponding attribute).
-- | - A raw attribute for an element (stringly typed, will be added directly to
-- |   the rendered element)
-- | - A key value used for hinting when diffing HTML.
-- | - An event handler.
-- | - A function that is triggered once the element for the property has
-- |   been added to or removed from the DOM.
data Prop i
  = Prop (Exists PropF)
  | Attr (Maybe Namespace) AttrName String
  | Key String
  | Handler EventType (Event -> Maybe i)
  | Ref (Maybe HTMLElement -> Maybe i)

instance functorProp :: Functor Prop where
  map _ (Prop e) = Prop e
  map _ (Key k) = Key k
  map _ (Attr ns k v) = Attr ns k v
  map f (Handler name k) = Handler name (map f <<< k)
  map f (Ref g) = Ref (map f <<< g)

-- | The data which represents a typed property, hidden inside an existential
-- | package in the `Prop` type.
data PropF value = PropF (PropName value) value (Maybe (Tuple AttrName (AttrName -> PropName value -> value -> String)))

-- | Create a HTML property.
prop :: forall value i. IsProp value => PropName value -> Maybe AttrName -> value -> Prop i
prop pn an v = Prop (mkExists (PropF pn v (flip Tuple toPropString <$> an)))

-- | Create a HTML attribute.
attr :: forall i. AttrName -> String -> Prop i
attr = Attr Nothing

-- | Create an event handler.
handler :: forall i. EventType -> (Event -> Maybe i) -> Prop i
handler = Handler

data FuseF p i x y = FuseF (x -> p) (y -> i) (HTML x y)

data Fuse p i

instance bifunctorFuse :: Bifunctor Fuse where
  bimap f g = unFuse \(FuseF k l html) ->
    mkFuse $ FuseF (f <<< k) (g <<< l) html

instance functorFuse :: Functor (Fuse p) where
  map = rmap

mkFuse :: forall p i x y. FuseF p i x y -> Fuse p i
mkFuse = unsafeCoerce

unFuse :: forall p i r. (forall x y. FuseF p i x y -> r) -> Fuse p i -> r
unFuse = unsafeCoerce

fuse :: forall p i x y. (x -> p) -> (y -> i) -> HTML x y -> Fuse p i
fuse k l html = mkFuse $ FuseF k l html

lowerFuse :: forall p i. Fuse p i -> HTML p i
lowerFuse = unFuse \(FuseF k l html) -> go k l html
  where
  go :: forall x y. (x -> p) -> (y -> i) -> HTML x y -> HTML p i
  go f g = case _ of
    Text s ->
      Text s
    Element ns name props els ->
      Element ns name (map g <$> props) (go f g <$> els)
    Slot p ->
      Slot (f p)
    Fuse h ->
      lowerFuse (bimap f g h)

-- | This type class captures those property types which can be used as
-- | attribute values.
-- |
-- | `toPropString` is an alternative to `show`, and is needed by `renderAttr`
-- | in the string renderer.
class IsProp a where
  toPropString :: AttrName -> PropName a -> a -> String

instance stringIsProp :: IsProp String where
  toPropString _ _ s = s

instance intIsProp :: IsProp Int where
  toPropString _ _ i = show i

instance numberIsProp :: IsProp Number where
  toPropString _ _ n = show n

instance booleanIsProp :: IsProp Boolean where
  toPropString (AttrName name) _ true = name
  toPropString _ _ false = ""

-- | A type-safe wrapper for a attribute or tag namespace.
newtype Namespace = Namespace String

derive instance newtypeNamespace :: Newtype Namespace _
derive newtype instance eqNamespace :: Eq Namespace
derive newtype instance ordNamespace :: Ord Namespace
derive instance genericNamespace :: Generic Namespace

-- | A type-safe wrapper for a HTML tag name
newtype TagName = TagName String

derive instance newtypeTagName :: Newtype TagName _
derive newtype instance eqTagName :: Eq TagName
derive newtype instance ordTagName :: Ord TagName
derive instance genericTagName :: Generic TagName

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
