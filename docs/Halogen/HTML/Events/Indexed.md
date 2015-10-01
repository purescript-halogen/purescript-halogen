## Module Halogen.HTML.Events.Indexed

#### `IEventProp`

``` purescript
type IEventProp ρ e i = (Event e -> EventHandler i) -> IProp ρ i
```

#### `onAbort`

``` purescript
onAbort :: forall ρ i. IEventProp (onAbort :: I | ρ) () i
```

#### `onBeforeUnload`

``` purescript
onBeforeUnload :: forall ρ i. IEventProp (onBeforeUnload :: I | ρ) () i
```

#### `onError`

``` purescript
onError :: forall ρ i. IEventProp (onError :: I | ρ) () i
```

#### `onHashChange`

``` purescript
onHashChange :: forall ρ i. IEventProp (onHashChange :: I | ρ) () i
```

#### `onLoad`

``` purescript
onLoad :: forall ρ i. IEventProp (onLoad :: I | ρ) () i
```

#### `onPageShow`

``` purescript
onPageShow :: forall ρ i. IEventProp (onPageShow :: I | ρ) () i
```

#### `onPageHide`

``` purescript
onPageHide :: forall ρ i. IEventProp (onPageHide :: I | ρ) () i
```

#### `onResize`

``` purescript
onResize :: forall ρ i. IEventProp (onResize :: I | ρ) () i
```

#### `onScroll`

``` purescript
onScroll :: forall ρ i. IEventProp (onScroll :: I | ρ) () i
```

#### `onUnload`

``` purescript
onUnload :: forall ρ i. IEventProp (onUnload :: I | ρ) () i
```

#### `onChange`

``` purescript
onChange :: forall ρ i. IEventProp (onChange :: I | ρ) () i
```

#### `onInput`

``` purescript
onInput :: forall ρ i. IEventProp (onInput :: I | ρ) () i
```

#### `onInvalid`

``` purescript
onInvalid :: forall ρ i. IEventProp (onInvalid :: I | ρ) () i
```

#### `onReset`

``` purescript
onReset :: forall ρ i. IEventProp (onReset :: I | ρ) () i
```

#### `onSearch`

``` purescript
onSearch :: forall ρ i. IEventProp (onSearch :: I | ρ) () i
```

#### `onSelect`

``` purescript
onSelect :: forall ρ i. IEventProp (onSelect :: I | ρ) () i
```

#### `onSubmit`

``` purescript
onSubmit :: forall ρ i. IEventProp (onSubmit :: I | ρ) () i
```

#### `onClick`

``` purescript
onClick :: forall ρ i. IEventProp (onClick :: I | ρ) MouseEvent i
```

#### `onContextMenu`

``` purescript
onContextMenu :: forall ρ i. IEventProp (onContextMenu :: I | ρ) MouseEvent i
```

#### `onDoubleClick`

``` purescript
onDoubleClick :: forall ρ i. IEventProp (onDoubleClick :: I | ρ) MouseEvent i
```

#### `onMouseDown`

``` purescript
onMouseDown :: forall ρ i. IEventProp (onMouseDown :: I | ρ) MouseEvent i
```

#### `onMouseLeave`

``` purescript
onMouseLeave :: forall ρ i. IEventProp (onMouseLeave :: I | ρ) MouseEvent i
```

#### `onMouseMove`

``` purescript
onMouseMove :: forall ρ i. IEventProp (onMouseMove :: I | ρ) MouseEvent i
```

#### `onMouseOver`

``` purescript
onMouseOver :: forall ρ i. IEventProp (onMouseOver :: I | ρ) MouseEvent i
```

#### `onMouseOut`

``` purescript
onMouseOut :: forall ρ i. IEventProp (onMouseOut :: I | ρ) MouseEvent i
```

#### `onMouseUp`

``` purescript
onMouseUp :: forall ρ i. IEventProp (onMouseUp :: I | ρ) MouseEvent i
```

#### `onKeyDown`

``` purescript
onKeyDown :: forall ρ i. IEventProp (onKeyDown :: I | ρ) KeyboardEvent i
```

#### `onKeyPress`

``` purescript
onKeyPress :: forall ρ i. IEventProp (onKeyPress :: I | ρ) KeyboardEvent i
```

#### `onKeyUp`

``` purescript
onKeyUp :: forall ρ i. IEventProp (onKeyUp :: I | ρ) KeyboardEvent i
```

#### `onBlur`

``` purescript
onBlur :: forall ρ i. IEventProp (onBlur :: I | ρ) FocusEvent i
```

#### `onFocus`

``` purescript
onFocus :: forall ρ i. IEventProp (onFocus :: I | ρ) FocusEvent i
```

#### `onFocusIn`

``` purescript
onFocusIn :: forall ρ i. IEventProp (onFocusIn :: I | ρ) FocusEvent i
```

#### `onFocusOut`

``` purescript
onFocusOut :: forall ρ i. IEventProp (onFocusOut :: I | ρ) FocusEvent i
```

#### `onValueChange`

``` purescript
onValueChange :: forall ρ f. (String -> EventHandler (f Unit)) -> IProp (value :: I, onChange :: I | ρ) (f Unit)
```

#### `onValueInput`

``` purescript
onValueInput :: forall ρ f. (String -> EventHandler (f Unit)) -> IProp (value :: I, onInput :: I | ρ) (f Unit)
```

#### `onChecked`

``` purescript
onChecked :: forall ρ f. (Boolean -> EventHandler (f Unit)) -> IProp (checked :: I, onChange :: I | ρ) (f Unit)
```


