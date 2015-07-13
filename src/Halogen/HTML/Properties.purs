-- | This module enumerates some common HTML attributes, and provides additional
-- | helper functions for working with CSS classes.

module Halogen.HTML.Properties
  ( Prop(..)
  , prop
  , handler
  , initializer
  , finalizer

  , AttrName()
  , attrName
  , runAttrName

  , AttrNS()
  , attrNS
  , runAttrNS

  , PropName()
  , propName
  , runPropName

  , ClassName()
  , className
  , runClassName

  , EventName()
  , eventName
  , runEventName

  , IsProp
  , toPropString

  , PropF(..)
  , HandlerF(..)

  , key
  , alt
  , charset
  , class_
  , classes
  , colSpan
  , rowSpan
  , for
  , height
  , href
  , id_
  , name
  , rel
  , src
  , target
  , title
  , type_
  , value
  , width
  , disabled
  , required
  , readonly
  , spellcheck
  , enabled
  , checked
  , selected
  , placeholder
  ) where

import Prelude

import Data.Either (either)
import Data.Exists (Exists(), mkExists)
import Data.ExistsR (ExistsR(), mkExistsR, runExistsR)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

import Halogen.HTML.Events.Handler (EventHandler())
import Halogen.HTML.Events.Types (Event())

-- | A single attribute is either
-- |
-- | - An attribute
-- | - An event handler
data Prop i
  = Prop (Exists PropF)
  | Attr (Maybe AttrNS) AttrName String
  | Handler (ExistsR (HandlerF i))
  | Initializer i
  | Finalizer i

instance functorProp :: Functor Prop where
  map _ (Prop e) = Prop e
  map _ (Attr ns k v) = Attr ns k v
  -- map f (Handler e) = runExistsR (\(HandlerF name k) -> Handler (mkExistsR (HandlerF name (\e -> f <$> k e)))) e
  map f (Initializer i) = Initializer (f i)
  map f (Finalizer i) = Finalizer (f i)

-- | Create an attribute
prop :: forall value i. (IsProp value) => PropName value -> Maybe AttrName -> value -> Prop i
prop name attr v = Prop (mkExists (PropF name v (flip Tuple toPropString <$> attr)))

-- | Create an event handler
handler :: forall fields i. EventName fields -> (Event fields -> EventHandler i) -> Prop i
handler name k = Handler (mkExistsR (HandlerF name k))

-- | Attach an initializer.
initializer :: forall i. i -> Prop i
initializer = Initializer

-- | Attach a finalizer.
finalizer :: forall i. i -> Prop i
finalizer = Finalizer

-- | The data which represents a typed attribute, hidden inside an existential package in
-- | the `Prop` type.
data PropF value = PropF (PropName value) value (Maybe (Tuple AttrName (AttrName -> PropName value -> value -> String)))

-- | The data which represents a typed event handler, hidden inside an existential package in
-- | the `Prop` type.
data HandlerF i fields = HandlerF (EventName fields) (Event fields -> EventHandler i)

-- | A type-safe wrapper for attribute names
-- |
-- | The phantom type `value` describes the type of value which this attribute requires.
newtype PropName value = PropName String

-- | Create an attribute name
propName :: forall value. String -> PropName value
propName = PropName

-- | Unpack an attribute name
runPropName :: forall value. PropName value -> String
runPropName (PropName s) = s

newtype AttrNS = AttrNS String

attrNS :: String -> AttrNS
attrNS = AttrNS

runAttrNS :: AttrNS -> String
runAttrNS (AttrNS ns) = ns

newtype AttrName = AttrName String

attrName :: String -> AttrName
attrName = AttrName

runAttrName :: AttrName -> String
runAttrName (AttrName ns) = ns

-- | A wrapper for strings which are used as CSS classes
newtype ClassName = ClassName String

-- Create a class name
className :: String -> ClassName
className = ClassName

-- | Unpack a class name
runClassName :: ClassName -> String
runClassName (ClassName s) = s

-- | A type-safe wrapper for event names.
-- |
-- | The phantom type `fields` describes the event type which we can expect to exist on events
-- | corresponding to this name.
newtype EventName (fields :: # *) = EventName String

-- Create an event name
eventName :: forall fields. String -> EventName fields
eventName = EventName

-- | Unpack an event name
runEventName :: forall fields. EventName fields -> String
runEventName (EventName s) = s

-- | This type class captures those types which can be used as attribute values.
-- |
-- | `toPropString` is an alternative to `show`, and is needed by `prop` in the string renderer.
class IsProp a where
  toPropString :: AttrName -> PropName a -> a -> String

instance stringIsProp :: IsProp String where
  toPropString _ _ s = s

instance intIsProp :: IsProp Int where
  toPropString _ _ i = show i

instance numberIsProp :: IsProp Number where
  toPropString _ _ n = show n

instance booleanIsProp :: IsProp Boolean where
  toPropString name _ true = runAttrName name
  toPropString _ _ false = ""

-- | The `key` property associates a unique key with a node, which can be used to
-- | implement a more efficient diff/patch.
key :: forall i. String -> Prop i
key = prop (propName "key") Nothing

alt :: forall i. String -> Prop i
alt = prop (propName "alt") (Just $ attrName "alt")

charset :: forall i. String -> Prop i
charset = prop (propName "charset") (Just $ attrName "charset")

class_ :: forall i. ClassName -> Prop i
class_ = prop (propName "className") (Just $ attrName "class") <<< runClassName

classes :: forall i. Array ClassName -> Prop i
classes = prop (propName "className") (Just $ attrName "class") <<< joinWith " " <<< map runClassName

colSpan :: forall i. Int -> Prop i
colSpan = prop (propName "colSpan") (Just $ attrName "colspan")

rowSpan :: forall i. Int -> Prop i
rowSpan = prop (propName "rowSpan") (Just $ attrName "rowspan")

for :: forall i. String -> Prop i
for = prop (propName "htmlFor") (Just $ attrName "for")

height :: forall i. Number -> Prop i
height = prop (propName "height") (Just $ attrName "height")

href :: forall i. String -> Prop i
href = prop (propName "href") (Just $ attrName "href")

id_ :: forall i. String -> Prop i
id_ = prop (propName "id") (Just $ attrName "id")

name :: forall i. String -> Prop i
name = prop (propName "name") (Just $ attrName "name")

rel :: forall i. String -> Prop i
rel = prop (propName "rel") (Just $ attrName "rel")

src :: forall i. String -> Prop i
src = prop (propName "src") (Just $ attrName "src")

target :: forall i. String -> Prop i
target = prop (propName "target") (Just $ attrName "target")

title :: forall i. String -> Prop i
title = prop (propName "title") (Just $ attrName "title")

type_ :: forall i. String -> Prop i
type_ = prop (propName "type") (Just $ attrName "type")

-- TODO: string rendering of value will need custom handling depending on element type
value :: forall i. String -> Prop i
value = prop (propName "value") (Just $ attrName "value")

width :: forall i. Number -> Prop i
width = prop (propName "width") (Just $ attrName "width")

disabled :: forall i. Boolean -> Prop i
disabled = prop (propName "disabled") (Just $ attrName "disabled")

required :: forall i. Boolean -> Prop i
required = prop (propName "required") (Just $ attrName "required")

readonly :: forall i. Boolean -> Prop i
readonly = prop (propName "readonly") (Just $ attrName "readonly")

spellcheck :: forall i. Boolean -> Prop i
spellcheck = prop (propName "spellcheck") (Just $ attrName "spellcheck")

enabled :: forall i. Boolean -> Prop i
enabled = disabled <<< not

checked :: forall i. Boolean -> Prop i
checked = prop (propName "checked") (Just $ attrName "checked")

selected :: forall i. Boolean -> Prop i
selected = prop (propName "selected") (Just $ attrName "selected")

placeholder :: forall i. String -> Prop i
placeholder = prop (propName "placeholder") (Just $ attrName "placeholder")
