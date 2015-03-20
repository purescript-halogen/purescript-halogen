# Module Documentation

## Module Halogen.HTML.Events.Types


This module defines types for common DOM events

#### `Event`

``` purescript
type Event fields = { "type" :: String, timeStamp :: Number, target :: Node, currentTarget :: Node, cancelable :: Boolean, bubbles :: Boolean | fields }
```

This record synonym captures the properties which appear on every DOM event.

The `fields` type parameter allows us to attach different types of additional
properties to represent more specific types of events.

#### `MouseEvent`

``` purescript
type MouseEvent = (which :: Number, metaKey :: Boolean, altKey :: Boolean, shiftKey :: Boolean, ctrlKey :: Boolean, screenY :: Number, screenX :: Number, clientY :: Number, clientX :: Number, relatedTarget :: Node, detail :: Number, button :: Number)
```

Identifies the additional fields which are available on mouse events.

#### `KeyboardEvent`

``` purescript
type KeyboardEvent = (which :: Number, metaKey :: Boolean, altKey :: Boolean, shiftKey :: Boolean, ctrlKey :: Boolean, keyCode :: Number, charCode :: Number)
```

Identifies the additional fields which are available on keyboard events.

#### `FocusEvent`

``` purescript
type FocusEvent = (relatedTarget :: Node)
```

Identifies the additional fields which are available on focus events.


## Module Halogen.HTML.Events


This module defines well-typed wrappers for common DOM events, so that
they may be safely embedded in HTML documents.

#### `createHandler`

``` purescript
createHandler :: forall fields attr i. (H.AttrRepr attr) => H.EventName fields -> (Event fields -> EventHandler i) -> attr i
```

This function can be used to attach custom event handlers.

#### `onabort`

``` purescript
onabort :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onbeforeunload`

``` purescript
onbeforeunload :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onerror`

``` purescript
onerror :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onhashchange`

``` purescript
onhashchange :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onload`

``` purescript
onload :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onpageshow`

``` purescript
onpageshow :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onpagehide`

``` purescript
onpagehide :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onresize`

``` purescript
onresize :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onscroll`

``` purescript
onscroll :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onunload`

``` purescript
onunload :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onchange`

``` purescript
onchange :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `oninput`

``` purescript
oninput :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `oninvalid`

``` purescript
oninvalid :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onreset`

``` purescript
onreset :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onsearch`

``` purescript
onsearch :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onselect`

``` purescript
onselect :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onsubmit`

``` purescript
onsubmit :: forall attr i. (H.AttrRepr attr) => (Event () -> EventHandler i) -> attr i
```


#### `onclick`

``` purescript
onclick :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
```


#### `oncontextmenu`

``` purescript
oncontextmenu :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
```


#### `ondblclick`

``` purescript
ondblclick :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
```


#### `onmousedown`

``` purescript
onmousedown :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
```


#### `onmouseenter`

``` purescript
onmouseenter :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
```


#### `onmouseleave`

``` purescript
onmouseleave :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
```


#### `onmousemove`

``` purescript
onmousemove :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
```


#### `onmouseover`

``` purescript
onmouseover :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
```


#### `onmouseout`

``` purescript
onmouseout :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
```


#### `onmouseup`

``` purescript
onmouseup :: forall attr i. (H.AttrRepr attr) => (Event MouseEvent -> EventHandler i) -> attr i
```


#### `onkeydown`

``` purescript
onkeydown :: forall attr i. (H.AttrRepr attr) => (Event KeyboardEvent -> EventHandler i) -> attr i
```


#### `onkeypress`

``` purescript
onkeypress :: forall attr i. (H.AttrRepr attr) => (Event KeyboardEvent -> EventHandler i) -> attr i
```


#### `onkeyup`

``` purescript
onkeyup :: forall attr i. (H.AttrRepr attr) => (Event KeyboardEvent -> EventHandler i) -> attr i
```


#### `onblur`

``` purescript
onblur :: forall attr i. (H.AttrRepr attr) => (Event FocusEvent -> EventHandler i) -> attr i
```


#### `onfocus`

``` purescript
onfocus :: forall attr i. (H.AttrRepr attr) => (Event FocusEvent -> EventHandler i) -> attr i
```


#### `onfocusin`

``` purescript
onfocusin :: forall attr i. (H.AttrRepr attr) => (Event FocusEvent -> EventHandler i) -> attr i
```


#### `onfocusout`

``` purescript
onfocusout :: forall attr i. (H.AttrRepr attr) => (Event FocusEvent -> EventHandler i) -> attr i
```



## Module Halogen.HTML.Events.Handler


This module defines the `EventHandler` functor, which can be used
to perform standard operations on HTML events.

#### `EventHandler`

``` purescript
data EventHandler a
```

This applicative functor supports the following operations on events:

- `preventDefault`
- `stopPropagation`
- `stopImmediatePropagation`

It can be used as follows:

```purescript
import Control.Functor (($>))

H.a (E.onclick \_ -> E.preventDefault $> ClickHandler) (H.text "Click here")
```

#### `preventDefault`

``` purescript
preventDefault :: EventHandler Unit
```

Call the `preventDefault` method on the current event

#### `stopPropagation`

``` purescript
stopPropagation :: EventHandler Unit
```

Call the `stopPropagation` method on the current event

#### `stopImmediatePropagation`

``` purescript
stopImmediatePropagation :: EventHandler Unit
```

Call the `stopImmediatePropagation` method on the current event

#### `functorEventHandler`

``` purescript
instance functorEventHandler :: Functor EventHandler
```


#### `applyEventHandler`

``` purescript
instance applyEventHandler :: Apply EventHandler
```


#### `applicativeEventHandler`

``` purescript
instance applicativeEventHandler :: Applicative EventHandler
```


#### `runEventHandler`

``` purescript
runEventHandler :: forall a fields eff. Event fields -> EventHandler a -> Eff (dom :: DOM | eff) a
```

This function can be used to update an event and return the wrapped value



