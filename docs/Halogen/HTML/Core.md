## Module Halogen.HTML.Core

The core types and smart constructors for the HTML DSL.

#### `HTML`

``` purescript
data HTML p i
  = Text String
  | Element (Maybe Namespace) TagName (Array (Prop i)) (Array (HTML p i))
  | Widget WidgetF
  | Placeholder p
```

An initial encoding of HTML nodes.

##### Instances
``` purescript
instance bifunctorHTML :: Bifunctor HTML
instance functorHTML :: Functor (HTML p)
```

#### `WidgetF`

``` purescript
data WidgetF
  = WidgetF { init :: forall eff. Eff (dom :: DOM | eff) HTMLElement, update :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) (Nullable HTMLElement), destroy :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit }
```

#### `element`

``` purescript
element :: forall p i. TagName -> Array (Prop i) -> Array (HTML p i) -> HTML p i
```

#### `install`

``` purescript
install :: forall p p' i i' m. (Applicative m) => (p -> m (HTML p' i')) -> (i -> i') -> HTML p i -> m (HTML p' i')
```

#### `Prop`

``` purescript
data Prop i
  = Prop (Exists PropF)
  | Attr (Maybe Namespace) AttrName String
  | Key String
  | Handler (ExistsR (HandlerF i))
  | Initializer (HTMLElement -> i)
  | Finalizer (HTMLElement -> i)
```

A property can be:
- A JavaScript property for an element (typed, and may not have a
  corresponding attribute).
- A raw attribute for an element (stringly typed, will be added directly to
  the rendered element)
- A key value used for hinting when diffing HTML.
- An event handler.
- A initializer that is triggered once the element for the property has
  been added to the DOM.
- A finalizer that is triggered when the element for the property is
  removed from the DOM.

##### Instances
``` purescript
instance functorProp :: Functor Prop
```

#### `PropF`

``` purescript
data PropF value
  = PropF (PropName value) value (Maybe (Tuple AttrName (AttrName -> PropName value -> value -> String)))
```

The data which represents a typed property, hidden inside an existential
package in the `Prop` type.

#### `HandlerF`

``` purescript
data HandlerF i fields
  = HandlerF (EventName fields) (Event fields -> EventHandler i)
```

The data which represents a typed event handler, hidden inside an
existential package in the `Prop` type.

#### `prop`

``` purescript
prop :: forall value i. (IsProp value) => PropName value -> Maybe AttrName -> value -> Prop i
```

Create an attribute

#### `handler`

``` purescript
handler :: forall fields i. EventName fields -> (Event fields -> EventHandler i) -> Prop i
```

Create an event handler

#### `IsProp`

``` purescript
class IsProp a where
  toPropString :: AttrName -> PropName a -> a -> String
```

This type class captures those property types which can be used as
attribute values.

`toPropString` is an alternative to `show`, and is needed by `renderAttr`
in the string renderer.

##### Instances
``` purescript
instance stringIsProp :: IsProp String
instance intIsProp :: IsProp Int
instance numberIsProp :: IsProp Number
instance booleanIsProp :: IsProp Boolean
```

#### `Namespace`

``` purescript
newtype Namespace
```

A type-safe wrapper for a attribute or tag namespace.

#### `namespace`

``` purescript
namespace :: String -> Namespace
```

Create a namespace

#### `runNamespace`

``` purescript
runNamespace :: Namespace -> String
```

Unwrap a `Namespace` to get the value as a `String`.

#### `TagName`

``` purescript
newtype TagName
```

A type-safe wrapper for a HTML tag name

#### `tagName`

``` purescript
tagName :: String -> TagName
```

Create a tag name

#### `runTagName`

``` purescript
runTagName :: TagName -> String
```

Unwrap a `TagName` to get the tag name as a `String`.

#### `PropName`

``` purescript
newtype PropName value
```

A type-safe wrapper for property names.

The phantom type `value` describes the type of value which this property requires.

#### `propName`

``` purescript
propName :: forall value. String -> PropName value
```

Create an attribute name

#### `runPropName`

``` purescript
runPropName :: forall value. PropName value -> String
```

Unpack an attribute name

#### `AttrName`

``` purescript
newtype AttrName
```

A type-safe wrapper for attribute names.

#### `attrName`

``` purescript
attrName :: String -> AttrName
```

#### `runAttrName`

``` purescript
runAttrName :: AttrName -> String
```

#### `EventName`

``` purescript
newtype EventName (fields :: # *)
```

A type-safe wrapper for event names.

The phantom type `fields` describes the event type which we can expect to exist on events
corresponding to this name.

#### `eventName`

``` purescript
eventName :: forall fields. String -> EventName fields
```

#### `runEventName`

``` purescript
runEventName :: forall fields. EventName fields -> String
```

Unpack an event name

#### `ClassName`

``` purescript
newtype ClassName
```

A wrapper for strings which are used as CSS classes.

#### `className`

``` purescript
className :: String -> ClassName
```

#### `runClassName`

``` purescript
runClassName :: ClassName -> String
```

Unpack a class name


