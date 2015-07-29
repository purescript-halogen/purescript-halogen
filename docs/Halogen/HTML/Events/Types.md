## Module Halogen.HTML.Events.Types

Common DOM event types.

#### `Event`

``` purescript
type Event fields = { bubbles :: Boolean, cancelable :: Boolean, currentTarget :: HTMLElement, target :: HTMLElement, timeStamp :: Number, type :: String | fields }
```

This record synonym captures the properties which appear on every DOM event.

The `fields` type parameter allows us to attach different types of additional
properties to represent more specific types of events.

#### `MouseEvent`

``` purescript
type MouseEvent = (button :: Number, detail :: Number, relatedTarget :: HTMLElement, clientX :: Number, clientY :: Number, screenX :: Number, screenY :: Number, ctrlKey :: Boolean, shiftKey :: Boolean, altKey :: Boolean, metaKey :: Boolean, which :: Number)
```

Identifies the additional fields which are available on mouse events.

#### `KeyboardEvent`

``` purescript
type KeyboardEvent = (charCode :: Number, keyCode :: Number, ctrlKey :: Boolean, shiftKey :: Boolean, altKey :: Boolean, metaKey :: Boolean, which :: Number)
```

Identifies the additional fields which are available on keyboard events.

#### `FocusEvent`

``` purescript
type FocusEvent = (relatedTarget :: HTMLElement)
```

Identifies the additional fields which are available on focus events.


