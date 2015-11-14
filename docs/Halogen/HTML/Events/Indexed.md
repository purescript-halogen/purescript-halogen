## Module Halogen.HTML.Events.Indexed

#### `IEventProp`

``` purescript
type IEventProp r e i = (Event e -> EventHandler i) -> IProp r i
```

#### `onAbort`

``` purescript
onAbort :: forall r i. IEventProp (onAbort :: I | r) () i
```

#### `onBeforeUnload`

``` purescript
onBeforeUnload :: forall r i. IEventProp (onBeforeUnload :: I | r) () i
```

#### `onError`

``` purescript
onError :: forall r i. IEventProp (onError :: I | r) () i
```

#### `onHashChange`

``` purescript
onHashChange :: forall r i. IEventProp (onHashChange :: I | r) () i
```

#### `onLoad`

``` purescript
onLoad :: forall r i. IEventProp (onLoad :: I | r) () i
```

#### `onPageShow`

``` purescript
onPageShow :: forall r i. IEventProp (onPageShow :: I | r) () i
```

#### `onPageHide`

``` purescript
onPageHide :: forall r i. IEventProp (onPageHide :: I | r) () i
```

#### `onResize`

``` purescript
onResize :: forall r i. IEventProp (onResize :: I | r) () i
```

#### `onScroll`

``` purescript
onScroll :: forall r i. IEventProp (onScroll :: I | r) () i
```

#### `onUnload`

``` purescript
onUnload :: forall r i. IEventProp (onUnload :: I | r) () i
```

#### `onChange`

``` purescript
onChange :: forall r i. IEventProp (onChange :: I | r) () i
```

#### `onInput`

``` purescript
onInput :: forall r i. IEventProp (onInput :: I | r) () i
```

#### `onInvalid`

``` purescript
onInvalid :: forall r i. IEventProp (onInvalid :: I | r) () i
```

#### `onReset`

``` purescript
onReset :: forall r i. IEventProp (onReset :: I | r) () i
```

#### `onSearch`

``` purescript
onSearch :: forall r i. IEventProp (onSearch :: I | r) () i
```

#### `onSelect`

``` purescript
onSelect :: forall r i. IEventProp (onSelect :: I | r) () i
```

#### `onSubmit`

``` purescript
onSubmit :: forall r i. IEventProp (onSubmit :: I | r) () i
```

#### `onClick`

``` purescript
onClick :: forall r i. IEventProp (onClick :: I | r) MouseEvent i
```

#### `onContextMenu`

``` purescript
onContextMenu :: forall r i. IEventProp (onContextMenu :: I | r) MouseEvent i
```

#### `onDoubleClick`

``` purescript
onDoubleClick :: forall r i. IEventProp (onDoubleClick :: I | r) MouseEvent i
```

#### `onMouseDown`

``` purescript
onMouseDown :: forall r i. IEventProp (onMouseDown :: I | r) MouseEvent i
```

#### `onMouseEnter`

``` purescript
onMouseEnter :: forall r i. IEventProp (onMouseEnter :: I | r) MouseEvent i
```

#### `onMouseLeave`

``` purescript
onMouseLeave :: forall r i. IEventProp (onMouseLeave :: I | r) MouseEvent i
```

#### `onMouseMove`

``` purescript
onMouseMove :: forall r i. IEventProp (onMouseMove :: I | r) MouseEvent i
```

#### `onMouseOver`

``` purescript
onMouseOver :: forall r i. IEventProp (onMouseOver :: I | r) MouseEvent i
```

#### `onMouseOut`

``` purescript
onMouseOut :: forall r i. IEventProp (onMouseOut :: I | r) MouseEvent i
```

#### `onMouseUp`

``` purescript
onMouseUp :: forall r i. IEventProp (onMouseUp :: I | r) MouseEvent i
```

#### `onKeyDown`

``` purescript
onKeyDown :: forall r i. IEventProp (onKeyDown :: I | r) KeyboardEvent i
```

#### `onKeyPress`

``` purescript
onKeyPress :: forall r i. IEventProp (onKeyPress :: I | r) KeyboardEvent i
```

#### `onKeyUp`

``` purescript
onKeyUp :: forall r i. IEventProp (onKeyUp :: I | r) KeyboardEvent i
```

#### `onBlur`

``` purescript
onBlur :: forall r i. IEventProp (onBlur :: I | r) FocusEvent i
```

#### `onFocus`

``` purescript
onFocus :: forall r i. IEventProp (onFocus :: I | r) FocusEvent i
```

#### `onFocusIn`

``` purescript
onFocusIn :: forall r i. IEventProp (onFocusIn :: I | r) FocusEvent i
```

#### `onFocusOut`

``` purescript
onFocusOut :: forall r i. IEventProp (onFocusOut :: I | r) FocusEvent i
```

#### `onValueChange`

``` purescript
onValueChange :: forall r f. (String -> EventHandler (f Unit)) -> IProp (value :: I, onChange :: I | r) (f Unit)
```

#### `onValueInput`

``` purescript
onValueInput :: forall r f. (String -> EventHandler (f Unit)) -> IProp (value :: I, onInput :: I | r) (f Unit)
```

#### `onChecked`

``` purescript
onChecked :: forall r f. (Boolean -> EventHandler (f Unit)) -> IProp (checked :: I, onChange :: I | r) (f Unit)
```


