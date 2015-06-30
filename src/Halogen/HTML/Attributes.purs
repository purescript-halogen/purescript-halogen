-- | This module enumerates some common HTML attributes, and provides additional
-- | helper functions for working with CSS classes.

module Halogen.HTML.Attributes 
  ( ClassName()
  , className
  , runClassName
  
  , AttributeName()
  , attributeName
  , runAttributeName
  
  , EventName()
  , eventName
  , runEventName
  
  , IsAttribute
  , toAttrString
  
  , ExistsR()
  , mkExistsR
  , runExistsR
  
  , AttrF(..)
  , HandlerF(..)
  
  , AttrNS(..)
  , attrNSRaw
  , Attr(..)
  
  , attr
  , attrNS
  , prop
  , handler
  , initializer
  , finalizer
  
  , key
  
  , alt
  , charset
  , class_
  , classes
  , colSpan
  , rowSpan
  , content
  , for
  , height
  , href
  , httpEquiv
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

import DOM

import Data.Maybe
import Data.Tuple
import Data.Either (either)
import Data.Foreign
import Data.Monoid (mempty)
import Data.Array (map)
import Data.String (joinWith)
import Data.Traversable (mapAccumL)
import Data.Exists

import Control.Monad.Eff
import Control.Monad.ST

import Halogen.Internal.VirtualDOM
import Halogen.HTML.Events.Types
import Halogen.HTML.Events.Handler

-- | We need a variant of `Exists` which works for type constructors which accept a _row_ of types.
data ExistsR (f :: # * -> *)

foreign import unsafeCoerce
  "function unsafeCoerce(x) {\
  \  return x;\
  \}" :: forall a b. a -> b
 
runExistsR :: forall f r. (forall a. f a -> r) -> ExistsR f -> r
runExistsR = unsafeCoerce
 
mkExistsR :: forall f a. f a -> ExistsR f
mkExistsR = unsafeCoerce

-- | The data which represents a typed attribute, hidden inside an existential package in
-- | the `Attr` type.
data AttrF value = AttrF (AttributeName value -> value -> String) (AttributeName value) value

-- | The data which represents a typed event handler, hidden inside an existential package in
-- | the `Attr` type.
data HandlerF i fields = HandlerF (EventName fields) (Event fields -> EventHandler i)

data AttrNS
  = SvgNS
  | XmlNS
  | XlinkNS

attrNSRaw :: AttrNS -> String
attrNSRaw SvgNS = ""
attrNSRaw XmlNS = "http://www.w3.org/XML/1998/namespace"
attrNSRaw XlinkNS = "http://www.w3.org/1999/xlink"

-- | A single attribute is either
-- |
-- | - An attribute
-- | - An event handler
data Attr i
  = Attr (Maybe AttrNS) (Exists AttrF)
  | Prop (Exists AttrF)
  | Handler (ExistsR (HandlerF i))
  | Initializer i
  | Finalizer i

instance functorAttr :: Functor Attr where
  (<$>) _ (Attr ns e) = Attr ns e
  (<$>) _ (Prop e) = Prop e
  (<$>) f (Handler e) = runExistsR (\(HandlerF name k) -> Handler (mkExistsR (HandlerF name (\e -> f <$> k e)))) e
  (<$>) f (Initializer i) = Initializer (f i)
  (<$>) f (Finalizer i) = Finalizer (f i)

-- | Create an attribute
attr :: forall value i. (IsAttribute value) => AttributeName value -> value -> Attr i
attr name v = Attr Nothing (mkExists (AttrF toAttrString name v))

-- | Create a namespaced attribute
attrNS :: forall value i. (IsAttribute value) => AttrNS -> AttributeName value -> value -> Attr i
attrNS ns name v = Attr (Just ns) (mkExists (AttrF toAttrString name v))

-- | Create a property
prop :: forall value i. (IsAttribute value) => AttributeName value -> value -> Attr i
prop name v = Prop (mkExists (AttrF toAttrString name v))

-- | Create an event handler
handler :: forall fields i. EventName fields -> (Event fields -> EventHandler i) -> Attr i
handler name k = Handler (mkExistsR (HandlerF name k))

-- | Attach an initializer.
initializer :: forall i. i -> Attr i
initializer = Initializer

-- | Attach a finalizer.
finalizer :: forall i. i -> Attr i
finalizer = Finalizer

-- | A wrapper for strings which are used as CSS classes
newtype ClassName = ClassName String

-- Create a class name
className :: String -> ClassName
className = ClassName

-- | Unpack a class name
runClassName :: ClassName -> String
runClassName (ClassName s) = s

-- | A type-safe wrapper for attribute names
-- |
-- | The phantom type `value` describes the type of value which this attribute requires.
newtype AttributeName value = AttributeName String

-- | Create an attribute name
attributeName :: forall value. String -> AttributeName value
attributeName = AttributeName

-- | Unpack an attribute name
runAttributeName :: forall value. AttributeName value -> String
runAttributeName (AttributeName s) = s

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
-- | `toAttrString` is an alternative to `show`, and is needed by `attr` in the string renderer.
class IsAttribute a where
  toAttrString :: AttributeName a -> a -> String
  
instance stringIsAttribute :: IsAttribute String where
  toAttrString _ s = s
  
instance numberIsAttribute :: IsAttribute Number where
  toAttrString _ n = show n
  
instance booleanIsAttribute :: IsAttribute Boolean where
  toAttrString name true = runAttributeName name
  toAttrString _ false = ""

-- Smart constructors

-- | The `key` property associates a unique key with a node, which can be used to 
-- | implement a more efficient diff/patch.
key :: forall i. String -> Attr i
key = attr $ attributeName "key"

alt :: forall i. String -> Attr i
alt = attr $ attributeName "alt"
     
charset :: forall i. String -> Attr i
charset = attr $ attributeName "charset"

class_ :: forall i. ClassName -> Attr i
class_ = attr (attributeName "className") <<< runClassName

classes :: forall i. [ClassName] -> Attr i
classes ss = attr (attributeName "className") (joinWith " " $ map runClassName ss)

colSpan :: forall i. Number -> Attr i
colSpan = attr (attributeName "colSpan") <<< show

rowSpan :: forall i. Number -> Attr i
rowSpan = attr (attributeName "rowSpan") <<< show

content :: forall i. String -> Attr i
content = attr $ attributeName "content"

for :: forall i. String -> Attr i
for = attr $ attributeName "htmlFor"

height :: forall i. Number -> Attr i
height = attr (attributeName "height") <<< show

href :: forall i. String -> Attr i
href = attr $ attributeName "href"

httpEquiv :: forall i. String -> Attr i
httpEquiv = attr $ attributeName "http-equiv"

id_ :: forall i. String -> Attr i
id_ = attr $ attributeName "id"
   
name :: forall i. String -> Attr i
name = attr $ attributeName "name"
       
rel :: forall i. String -> Attr i
rel = attr $ attributeName "rel"
    
src :: forall i. String -> Attr i
src = attr $ attributeName "src"
   
target :: forall i. String -> Attr i
target = attr $ attributeName "target"
   
title :: forall i. String -> Attr i
title = attr $ attributeName "title"
   
type_ :: forall i. String -> Attr i
type_ = attr $ attributeName "type"
   
value :: forall i. String -> Attr i
value = attr $ attributeName "value"
   
width :: forall i. Number -> Attr i
width = attr (attributeName "width") <<< show
   
disabled :: forall i. Boolean -> Attr i
disabled = attr $ attributeName "disabled"

required :: forall i. Boolean -> Attr i
required = attr $ attributeName "required"

readonly :: forall i. Boolean -> Attr i
readonly = attr $ attributeName "readonly"

spellcheck :: forall i. Boolean -> Attr i
spellcheck = attr $ attributeName "spellcheck"
   
enabled :: forall i. Boolean -> Attr i
enabled = disabled <<< not
   
checked :: forall i. Boolean -> Attr i
checked = attr $ attributeName "checked"
   
selected :: forall i. Boolean -> Attr i
selected = attr $ attributeName "selected"
   
placeholder :: forall i. String -> Attr i
placeholder = attr $ attributeName "placeholder"
