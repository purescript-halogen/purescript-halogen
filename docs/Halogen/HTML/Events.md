## Module Halogen.HTML.Events

This module defines well-typed wrappers for common DOM events, so that
they may be safely embedded in HTML documents.

#### `EventProp`

``` purescript
type EventProp e i = (Event e -> EventHandler i) -> Prop i
```

#### `input`

``` purescript
input :: forall f a. (a -> Action f) -> a -> EventHandler (f Unit)
```

#### `input_`

``` purescript
input_ :: forall f a. Action f -> a -> EventHandler (f Unit)
```

#### `onAbort`

``` purescript
onAbort :: forall i. EventProp () i
```

#### `onBeforeUnload`

``` purescript
onBeforeUnload :: forall i. EventProp () i
```

#### `onError`

``` purescript
onError :: forall i. EventProp () i
```

#### `onHashChange`

``` purescript
onHashChange :: forall i. EventProp () i
```

#### `onLoad`

``` purescript
onLoad :: forall i. EventProp () i
```

#### `onPageShow`

``` purescript
onPageShow :: forall i. EventProp () i
```

#### `onPageHide`

``` purescript
onPageHide :: forall i. EventProp () i
```

#### `onResize`

``` purescript
onResize :: forall i. EventProp () i
```

#### `onScroll`

``` purescript
onScroll :: forall i. EventProp () i
```

#### `onUnload`

``` purescript
onUnload :: forall i. EventProp () i
```

#### `onChange`

``` purescript
onChange :: forall i. EventProp () i
```

#### `onInput`

``` purescript
onInput :: forall i. EventProp () i
```

#### `onInvalid`

``` purescript
onInvalid :: forall i. EventProp () i
```

#### `onReset`

``` purescript
onReset :: forall i. EventProp () i
```

#### `onSearch`

``` purescript
onSearch :: forall i. EventProp () i
```

#### `onSelect`

``` purescript
onSelect :: forall i. EventProp () i
```

#### `onSubmit`

``` purescript
onSubmit :: forall i. EventProp () i
```

#### `onClick`

``` purescript
onClick :: forall i. EventProp MouseEvent i
```

#### `onContextMenu`

``` purescript
onContextMenu :: forall i. EventProp MouseEvent i
```

#### `onDoubleClick`

``` purescript
onDoubleClick :: forall i. EventProp MouseEvent i
```

#### `onMouseDown`

``` purescript
onMouseDown :: forall i. EventProp MouseEvent i
```

#### `onMouseEnter`

``` purescript
onMouseEnter :: forall i. EventProp MouseEvent i
```

#### `onMouseLeave`

``` purescript
onMouseLeave :: forall i. EventProp MouseEvent i
```

#### `onMouseMove`

``` purescript
onMouseMove :: forall i. EventProp MouseEvent i
```

#### `onMouseOver`

``` purescript
onMouseOver :: forall i. EventProp MouseEvent i
```

#### `onMouseOut`

``` purescript
onMouseOut :: forall i. EventProp MouseEvent i
```

#### `onMouseUp`

``` purescript
onMouseUp :: forall i. EventProp MouseEvent i
```

#### `onKeyDown`

``` purescript
onKeyDown :: forall i. EventProp KeyboardEvent i
```

#### `onKeyPress`

``` purescript
onKeyPress :: forall i. EventProp KeyboardEvent i
```

#### `onKeyUp`

``` purescript
onKeyUp :: forall i. EventProp KeyboardEvent i
```

#### `onBlur`

``` purescript
onBlur :: forall i. EventProp FocusEvent i
```

#### `onFocus`

``` purescript
onFocus :: forall i. EventProp FocusEvent i
```

#### `onFocusIn`

``` purescript
onFocusIn :: forall i. EventProp FocusEvent i
```

#### `onFocusOut`

``` purescript
onFocusOut :: forall i. EventProp FocusEvent i
```


