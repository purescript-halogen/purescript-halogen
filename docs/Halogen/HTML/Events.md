## Module Halogen.HTML.Events

This module defines well-typed wrappers for common DOM events, so that
they may be safely embedded in HTML documents.

#### `input`

``` purescript
input :: forall f a. (forall i. a -> i -> f i) -> a -> EventHandler (f Unit)
```

#### `input_`

``` purescript
input_ :: forall f a. (forall i. i -> f i) -> a -> EventHandler (f Unit)
```

#### `onAbort`

``` purescript
onAbort :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onBeforeUnload`

``` purescript
onBeforeUnload :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onError`

``` purescript
onError :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onHashChange`

``` purescript
onHashChange :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onLoad`

``` purescript
onLoad :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onPageShow`

``` purescript
onPageShow :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onPageHide`

``` purescript
onPageHide :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onResize`

``` purescript
onResize :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onScroll`

``` purescript
onScroll :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onUnload`

``` purescript
onUnload :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onChange`

``` purescript
onChange :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onInput`

``` purescript
onInput :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onInvalid`

``` purescript
onInvalid :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onReset`

``` purescript
onReset :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onSearch`

``` purescript
onSearch :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onSelect`

``` purescript
onSelect :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onSubmit`

``` purescript
onSubmit :: forall i. (Event () -> EventHandler i) -> Prop i
```

#### `onClick`

``` purescript
onClick :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
```

#### `onContextMenu`

``` purescript
onContextMenu :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
```

#### `onDoubleClick`

``` purescript
onDoubleClick :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
```

#### `onMouseDown`

``` purescript
onMouseDown :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
```

#### `onMouseEnter`

``` purescript
onMouseEnter :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
```

#### `onMouseLeave`

``` purescript
onMouseLeave :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
```

#### `onMouseMove`

``` purescript
onMouseMove :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
```

#### `onMouseOver`

``` purescript
onMouseOver :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
```

#### `onMouseOut`

``` purescript
onMouseOut :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
```

#### `onMouseUp`

``` purescript
onMouseUp :: forall i. (Event MouseEvent -> EventHandler i) -> Prop i
```

#### `onKeyDown`

``` purescript
onKeyDown :: forall i. (Event KeyboardEvent -> EventHandler i) -> Prop i
```

#### `onKeyPress`

``` purescript
onKeyPress :: forall i. (Event KeyboardEvent -> EventHandler i) -> Prop i
```

#### `onKeyUp`

``` purescript
onKeyUp :: forall i. (Event KeyboardEvent -> EventHandler i) -> Prop i
```

#### `onBlur`

``` purescript
onBlur :: forall i. (Event FocusEvent -> EventHandler i) -> Prop i
```

#### `onFocus`

``` purescript
onFocus :: forall i. (Event FocusEvent -> EventHandler i) -> Prop i
```

#### `onFocusIn`

``` purescript
onFocusIn :: forall i. (Event FocusEvent -> EventHandler i) -> Prop i
```

#### `onFocusOut`

``` purescript
onFocusOut :: forall i. (Event FocusEvent -> EventHandler i) -> Prop i
```


