## Module Halogen.HTML.Properties

This module provides `Prop` values for some common HTML attributes.

#### `LengthLiteral`

``` purescript
data LengthLiteral
  = Pixels Int
  | Percent Number
```

#### `key`

``` purescript
key :: forall i. String -> Prop i
```

The `key` property associates a unique key with a node, which can be used
to implement a more efficient diff/patch.

#### `ariaActivedescendant`

``` purescript
ariaActivedescendant :: forall i. String -> Prop i
```

#### `ariaAtomic`

``` purescript
ariaAtomic :: forall i. String -> Prop i
```

#### `ariaAutocomplete`

``` purescript
ariaAutocomplete :: forall i. String -> Prop i
```

#### `ariaBusy`

``` purescript
ariaBusy :: forall i. String -> Prop i
```

#### `ariaChecked`

``` purescript
ariaChecked :: forall i. String -> Prop i
```

#### `ariaControls`

``` purescript
ariaControls :: forall i. String -> Prop i
```

#### `ariaDescribedby`

``` purescript
ariaDescribedby :: forall i. String -> Prop i
```

#### `ariaDisabled`

``` purescript
ariaDisabled :: forall i. String -> Prop i
```

#### `ariaDropeffect`

``` purescript
ariaDropeffect :: forall i. String -> Prop i
```

#### `ariaExpanded`

``` purescript
ariaExpanded :: forall i. String -> Prop i
```

#### `ariaFlowto`

``` purescript
ariaFlowto :: forall i. String -> Prop i
```

#### `ariaGrabbed`

``` purescript
ariaGrabbed :: forall i. String -> Prop i
```

#### `ariaHaspopup`

``` purescript
ariaHaspopup :: forall i. String -> Prop i
```

#### `ariaHidden`

``` purescript
ariaHidden :: forall i. String -> Prop i
```

#### `ariaInvalid`

``` purescript
ariaInvalid :: forall i. String -> Prop i
```

#### `ariaLabel`

``` purescript
ariaLabel :: forall i. String -> Prop i
```

#### `ariaLabelledby`

``` purescript
ariaLabelledby :: forall i. String -> Prop i
```

#### `ariaLevel`

``` purescript
ariaLevel :: forall i. String -> Prop i
```

#### `ariaLive`

``` purescript
ariaLive :: forall i. String -> Prop i
```

#### `ariaMultiline`

``` purescript
ariaMultiline :: forall i. String -> Prop i
```

#### `ariaMultiselectable`

``` purescript
ariaMultiselectable :: forall i. String -> Prop i
```

#### `ariaOrientation`

``` purescript
ariaOrientation :: forall i. String -> Prop i
```

#### `ariaOwns`

``` purescript
ariaOwns :: forall i. String -> Prop i
```

#### `ariaPosinset`

``` purescript
ariaPosinset :: forall i. String -> Prop i
```

#### `ariaPressed`

``` purescript
ariaPressed :: forall i. String -> Prop i
```

#### `ariaReadonly`

``` purescript
ariaReadonly :: forall i. String -> Prop i
```

#### `ariaRelevant`

``` purescript
ariaRelevant :: forall i. String -> Prop i
```

#### `ariaRequired`

``` purescript
ariaRequired :: forall i. String -> Prop i
```

#### `ariaSelected`

``` purescript
ariaSelected :: forall i. String -> Prop i
```

#### `ariaSetsize`

``` purescript
ariaSetsize :: forall i. String -> Prop i
```

#### `ariaSort`

``` purescript
ariaSort :: forall i. String -> Prop i
```

#### `ariaValuemax`

``` purescript
ariaValuemax :: forall i. String -> Prop i
```

#### `ariaValuemin`

``` purescript
ariaValuemin :: forall i. String -> Prop i
```

#### `ariaValuenow`

``` purescript
ariaValuenow :: forall i. String -> Prop i
```

#### `ariaValuetext`

``` purescript
ariaValuetext :: forall i. String -> Prop i
```

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

#### `cols`

``` purescript
cols :: forall i. Int -> Prop i
```

#### `rows`

``` purescript
rows :: forall i. Int -> Prop i
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
height :: forall i. LengthLiteral -> Prop i
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
width :: forall i. LengthLiteral -> Prop i
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

#### `autocomplete`

``` purescript
autocomplete :: forall i. Boolean -> Prop i
```

#### `autofocus`

``` purescript
autofocus :: forall i. Boolean -> Prop i
```

#### `initializer`

``` purescript
initializer :: forall i. (HTMLElement -> i) -> Prop i
```

#### `finalizer`

``` purescript
finalizer :: forall i. (HTMLElement -> i) -> Prop i
```


