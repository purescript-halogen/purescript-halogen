## Module Halogen.HTML.Properties.Indexed

A closed signature of type-indexed (refined) HTML properties; these can be
used to ensure correctness by construction, and then erased into the standard
unrefined versions.

#### `IProp`

``` purescript
newtype IProp (r :: # *) i
  = IProp (Prop i)
```

The phantom row `r` can be thought of as a context which is synthesized in the
course of constructing a refined HTML expression.

#### `I`

``` purescript
data I
```

A dummy type to use in the phantom row.

#### `key`

``` purescript
key :: forall r i. String -> IProp (key :: I | r) i
```

#### `ariaActivedescendant`

``` purescript
ariaActivedescendant :: forall r i. String -> IProp (ariaActivedescendant :: I | r) i
```

#### `ariaAtomic`

``` purescript
ariaAtomic :: forall r i. String -> IProp (ariaAtomic :: I | r) i
```

#### `ariaAutocomplete`

``` purescript
ariaAutocomplete :: forall r i. String -> IProp (ariaAutocomplete :: I | r) i
```

#### `ariaBusy`

``` purescript
ariaBusy :: forall r i. String -> IProp (ariaBusy :: I | r) i
```

#### `ariaChecked`

``` purescript
ariaChecked :: forall r i. String -> IProp (ariaChecked :: I | r) i
```

#### `ariaControls`

``` purescript
ariaControls :: forall r i. String -> IProp (ariaControls :: I | r) i
```

#### `ariaDescribedby`

``` purescript
ariaDescribedby :: forall r i. String -> IProp (ariaDescribedby :: I | r) i
```

#### `ariaDisabled`

``` purescript
ariaDisabled :: forall r i. String -> IProp (ariaDisabled :: I | r) i
```

#### `ariaDropeffect`

``` purescript
ariaDropeffect :: forall r i. String -> IProp (ariaDropeffect :: I | r) i
```

#### `ariaExpanded`

``` purescript
ariaExpanded :: forall r i. String -> IProp (ariaExpanded :: I | r) i
```

#### `ariaFlowto`

``` purescript
ariaFlowto :: forall r i. String -> IProp (ariaFlowto :: I | r) i
```

#### `ariaGrabbed`

``` purescript
ariaGrabbed :: forall r i. String -> IProp (ariaGrabbed :: I | r) i
```

#### `ariaHaspopup`

``` purescript
ariaHaspopup :: forall r i. String -> IProp (ariaHaspopup :: I | r) i
```

#### `ariaHidden`

``` purescript
ariaHidden :: forall r i. String -> IProp (ariaHidden :: I | r) i
```

#### `ariaInvalid`

``` purescript
ariaInvalid :: forall r i. String -> IProp (ariaInvalid :: I | r) i
```

#### `ariaLabel`

``` purescript
ariaLabel :: forall r i. String -> IProp (ariaLabel :: I | r) i
```

#### `ariaLabelledby`

``` purescript
ariaLabelledby :: forall r i. String -> IProp (ariaLabelledby :: I | r) i
```

#### `ariaLevel`

``` purescript
ariaLevel :: forall r i. String -> IProp (ariaLevel :: I | r) i
```

#### `ariaLive`

``` purescript
ariaLive :: forall r i. String -> IProp (ariaLive :: I | r) i
```

#### `ariaMultiline`

``` purescript
ariaMultiline :: forall r i. String -> IProp (ariaMultiline :: I | r) i
```

#### `ariaMultiselectable`

``` purescript
ariaMultiselectable :: forall r i. String -> IProp (ariaMultiselectable :: I | r) i
```

#### `ariaOrientation`

``` purescript
ariaOrientation :: forall r i. String -> IProp (ariaOrientation :: I | r) i
```

#### `ariaOwns`

``` purescript
ariaOwns :: forall r i. String -> IProp (ariaOwns :: I | r) i
```

#### `ariaPosinset`

``` purescript
ariaPosinset :: forall r i. String -> IProp (ariaPosinset :: I | r) i
```

#### `ariaPressed`

``` purescript
ariaPressed :: forall r i. String -> IProp (ariaPressed :: I | r) i
```

#### `ariaReadonly`

``` purescript
ariaReadonly :: forall r i. String -> IProp (ariaReadonly :: I | r) i
```

#### `ariaRelevant`

``` purescript
ariaRelevant :: forall r i. String -> IProp (ariaRelevant :: I | r) i
```

#### `ariaRequired`

``` purescript
ariaRequired :: forall r i. String -> IProp (ariaRequired :: I | r) i
```

#### `ariaSelected`

``` purescript
ariaSelected :: forall r i. String -> IProp (ariaSelected :: I | r) i
```

#### `ariaSetsize`

``` purescript
ariaSetsize :: forall r i. String -> IProp (ariaSetsize :: I | r) i
```

#### `ariaSort`

``` purescript
ariaSort :: forall r i. String -> IProp (ariaSort :: I | r) i
```

#### `ariaValuemax`

``` purescript
ariaValuemax :: forall r i. String -> IProp (ariaValuemax :: I | r) i
```

#### `ariaValuemin`

``` purescript
ariaValuemin :: forall r i. String -> IProp (ariaValuemin :: I | r) i
```

#### `ariaValuenow`

``` purescript
ariaValuenow :: forall r i. String -> IProp (ariaValuenow :: I | r) i
```

#### `ariaValuetext`

``` purescript
ariaValuetext :: forall r i. String -> IProp (ariaValuetext :: I | r) i
```

#### `alt`

``` purescript
alt :: forall r i. String -> IProp (alt :: I | r) i
```

#### `charset`

``` purescript
charset :: forall r i. String -> IProp (charset :: I | r) i
```

#### `class_`

``` purescript
class_ :: forall r i. ClassName -> IProp (class :: I | r) i
```

#### `classes`

``` purescript
classes :: forall r i. Array ClassName -> IProp (class :: I | r) i
```

#### `cols`

``` purescript
cols :: forall r i. Int -> IProp (cols :: I | r) i
```

#### `rows`

``` purescript
rows :: forall r i. Int -> IProp (rows :: I | r) i
```

#### `colSpan`

``` purescript
colSpan :: forall r i. Int -> IProp (colSpan :: I | r) i
```

#### `rowSpan`

``` purescript
rowSpan :: forall r i. Int -> IProp (rowSpan :: I | r) i
```

#### `for`

``` purescript
for :: forall r i. String -> IProp (for :: I | r) i
```

#### `height`

``` purescript
height :: forall r i. LengthLiteral -> IProp (height :: I | r) i
```

#### `width`

``` purescript
width :: forall r i. LengthLiteral -> IProp (width :: I | r) i
```

#### `href`

``` purescript
href :: forall r i. String -> IProp (href :: I | r) i
```

#### `id_`

``` purescript
id_ :: forall r i. String -> IProp (id :: I | r) i
```

#### `name`

``` purescript
name :: forall r i. String -> IProp (name :: I | r) i
```

#### `rel`

``` purescript
rel :: forall r i. String -> IProp (rel :: I | r) i
```

#### `src`

``` purescript
src :: forall r i. String -> IProp (src :: I | r) i
```

#### `target`

``` purescript
target :: forall r i. String -> IProp (target :: I | r) i
```

#### `title`

``` purescript
title :: forall r i. String -> IProp (title :: I | r) i
```

#### `InputType`

``` purescript
data InputType
  = InputButton
  | InputCheckbox
  | InputColor
  | InputDate
  | InputDatetime
  | InputDatetimeLocal
  | InputEmail
  | InputFile
  | InputHidden
  | InputImage
  | InputMonth
  | InputNumber
  | InputPassword
  | InputRadio
  | InputRange
  | InputReset
  | InputSearch
  | InputSubmit
  | InputTel
  | InputText
  | InputTime
  | InputUrl
  | InputWeek
```

#### `inputType`

``` purescript
inputType :: forall r i. InputType -> IProp (inputType :: I | r) i
```

#### `MenuType`

``` purescript
data MenuType
  = MenuList
  | MenuContext
  | MenuToolbar
```

#### `menuType`

``` purescript
menuType :: forall r i. MenuType -> IProp (menuType :: I | r) i
```

#### `MenuitemType`

``` purescript
data MenuitemType
  = MenuitemCommand
  | MenuitemCheckbox
  | MenuitemRadio
```

#### `menuitemType`

``` purescript
menuitemType :: forall r i. MenuitemType -> IProp (menuitemType :: I | r) i
```

#### `MediaType`

``` purescript
type MediaType = { type :: String, subtype :: String, parameters :: Array (Tuple String String) }
```

#### `mediaType`

``` purescript
mediaType :: forall r i. MediaType -> IProp (mediaType :: I | r) i
```

#### `ButtonType`

``` purescript
data ButtonType
  = ButtonButton
  | ButtonSubmit
  | ButtonReset
```

#### `buttonType`

``` purescript
buttonType :: forall r i. ButtonType -> IProp (buttonType :: I | r) i
```

#### `CaseType`

``` purescript
data CaseType
  = Uppercase
  | Lowercase
```

#### `NumeralType`

``` purescript
data NumeralType
  = NumeralDecimal
  | NumeralRoman CaseType
```

#### `OrderedListType`

``` purescript
data OrderedListType
  = OrderedListNumeric NumeralType
  | OrderedListAlphabetic CaseType
```

#### `olType`

``` purescript
olType :: forall r i. OrderedListType -> IProp (olType :: I | r) i
```

#### `value`

``` purescript
value :: forall r i. String -> IProp (value :: I | r) i
```

#### `disabled`

``` purescript
disabled :: forall r i. Boolean -> IProp (disabled :: I | r) i
```

#### `required`

``` purescript
required :: forall r i. Boolean -> IProp (required :: I | r) i
```

#### `readonly`

``` purescript
readonly :: forall r i. Boolean -> IProp (readonly :: I | r) i
```

#### `spellcheck`

``` purescript
spellcheck :: forall r i. Boolean -> IProp (spellcheck :: I | r) i
```

#### `enabled`

``` purescript
enabled :: forall r i. Boolean -> IProp (disabled :: I | r) i
```

#### `checked`

``` purescript
checked :: forall r i. Boolean -> IProp (checked :: I | r) i
```

#### `selected`

``` purescript
selected :: forall r i. Boolean -> IProp (selected :: I | r) i
```

#### `placeholder`

``` purescript
placeholder :: forall r i. String -> IProp (placeholder :: I | r) i
```

#### `autocomplete`

``` purescript
autocomplete :: forall r i. Boolean -> IProp (autocomplete :: I | r) i
```

#### `autofocus`

``` purescript
autofocus :: forall r i. Boolean -> IProp (autofocus :: I | r) i
```

#### `initializer`

``` purescript
initializer :: forall r i. (HTMLElement -> i) -> IProp (initializer :: I | r) i
```

#### `finalizer`

``` purescript
finalizer :: forall r i. (HTMLElement -> i) -> IProp (finalizer :: I | r) i
```

#### `GlobalAttributes`

``` purescript
type GlobalAttributes r = (id :: I, name :: I, title :: I, class :: I, style :: I, spellcheck :: I, key :: I, initializer :: I, finalizer :: I | r)
```

#### `GlobalEvents`

``` purescript
type GlobalEvents r = (onContextMenu :: I | r)
```

#### `MouseEvents`

``` purescript
type MouseEvents r = (onDoubleClick :: I, onClick :: I, onMouseDown :: I, onMouseEnter :: I, onMouseLeave :: I, onMouseMove :: I, onMouseOver :: I, onMouseOut :: I, onMouseUp :: I | r)
```

#### `KeyEvents`

``` purescript
type KeyEvents r = (onKeyDown :: I, onKeyUp :: I, onKeyPress :: I | r)
```

#### `FocusEvents`

``` purescript
type FocusEvents r = (onBlur :: I, onFocus :: I, onFocusIn :: I, onFocusOut :: I | r)
```

#### `InteractiveEvents`

``` purescript
type InteractiveEvents r = FocusEvents (KeyEvents (MouseEvents r))
```

#### `GlobalProperties`

``` purescript
type GlobalProperties r = GlobalAttributes (GlobalEvents r)
```


