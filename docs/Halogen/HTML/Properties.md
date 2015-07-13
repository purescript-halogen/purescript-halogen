## Module Halogen.HTML.Properties

This module enumerates some common HTML attributes, and provides additional
helper functions for working with CSS classes.

#### `Prop`

``` purescript
data Prop i
  = Prop (Exists PropF)
  | Attr (Maybe AttrNS) AttrName String
  | Handler (ExistsR (HandlerF i))
  | Initializer i
  | Finalizer i
```

A single attribute is either

- An attribute
- An event handler

##### Instances
``` purescript
instance functorProp :: Functor Prop
```

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

#### `initializer`

``` purescript
initializer :: forall i. i -> Prop i
```

Attach an initializer.

#### `finalizer`

``` purescript
finalizer :: forall i. i -> Prop i
```

Attach a finalizer.

#### `PropF`

``` purescript
data PropF value
  = PropF (PropName value) value (Maybe (Tuple AttrName (AttrName -> PropName value -> value -> String)))
```

The data which represents a typed attribute, hidden inside an existential package in
the `Prop` type.

#### `HandlerF`

``` purescript
data HandlerF i fields
  = HandlerF (EventName fields) (Event fields -> EventHandler i)
```

The data which represents a typed event handler, hidden inside an existential package in
the `Prop` type.

#### `PropName`

``` purescript
newtype PropName value
```

A type-safe wrapper for attribute names

The phantom type `value` describes the type of value which this attribute requires.

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

#### `AttrNS`

``` purescript
newtype AttrNS
```

#### `attrNS`

``` purescript
attrNS :: String -> AttrNS
```

#### `runAttrNS`

``` purescript
runAttrNS :: AttrNS -> String
```

#### `AttrName`

``` purescript
newtype AttrName
```

#### `attrName`

``` purescript
attrName :: String -> AttrName
```

#### `runAttrName`

``` purescript
runAttrName :: AttrName -> String
```

#### `ClassName`

``` purescript
newtype ClassName
```

A wrapper for strings which are used as CSS classes

#### `className`

``` purescript
className :: String -> ClassName
```

#### `runClassName`

``` purescript
runClassName :: ClassName -> String
```

Unpack a class name

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

#### `IsProp`

``` purescript
class IsProp a where
  toPropString :: AttrName -> PropName a -> a -> String
```

This type class captures those types which can be used as attribute values.

`toPropString` is an alternative to `show`, and is needed by `prop` in the string renderer.

##### Instances
``` purescript
instance stringIsProp :: IsProp String
instance intIsProp :: IsProp Int
instance numberIsProp :: IsProp Number
instance booleanIsProp :: IsProp Boolean
```

#### `key`

``` purescript
key :: forall i. String -> Prop i
```

The `key` property associates a unique key with a node, which can be used to
implement a more efficient diff/patch.

#### `alt`

``` purescript
alt :: forall i. String -> Prop i
```

#### `charset`

``` purescript
charset :: forall i. String -> Prop i
```

#### `class_`

``` purescript
class_ :: forall i. ClassName -> Prop i
```

#### `classes`

``` purescript
classes :: forall i. Array ClassName -> Prop i
```

#### `colSpan`

``` purescript
colSpan :: forall i. Int -> Prop i
```

#### `rowSpan`

``` purescript
rowSpan :: forall i. Int -> Prop i
```

#### `for`

``` purescript
for :: forall i. String -> Prop i
```

#### `height`

``` purescript
height :: forall i. Number -> Prop i
```

#### `href`

``` purescript
href :: forall i. String -> Prop i
```

#### `id_`

``` purescript
id_ :: forall i. String -> Prop i
```

#### `name`

``` purescript
name :: forall i. String -> Prop i
```

#### `rel`

``` purescript
rel :: forall i. String -> Prop i
```

#### `src`

``` purescript
src :: forall i. String -> Prop i
```

#### `target`

``` purescript
target :: forall i. String -> Prop i
```

#### `title`

``` purescript
title :: forall i. String -> Prop i
```

#### `type_`

``` purescript
type_ :: forall i. String -> Prop i
```

#### `value`

``` purescript
value :: forall i. String -> Prop i
```

#### `width`

``` purescript
width :: forall i. Number -> Prop i
```

#### `disabled`

``` purescript
disabled :: forall i. Boolean -> Prop i
```

#### `required`

``` purescript
required :: forall i. Boolean -> Prop i
```

#### `readonly`

``` purescript
readonly :: forall i. Boolean -> Prop i
```

#### `spellcheck`

``` purescript
spellcheck :: forall i. Boolean -> Prop i
```

#### `enabled`

``` purescript
enabled :: forall i. Boolean -> Prop i
```

#### `checked`

``` purescript
checked :: forall i. Boolean -> Prop i
```

#### `selected`

``` purescript
selected :: forall i. Boolean -> Prop i
```

#### `placeholder`

``` purescript
placeholder :: forall i. String -> Prop i
```


