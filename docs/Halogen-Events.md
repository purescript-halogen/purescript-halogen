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
createHandler :: forall fields i. H.EventName fields -> (Event fields -> EventHandler i) -> H.Attr i
```

This function can be used to attach custom event handlers.

#### `onabort`

``` purescript
onabort :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onbeforeunload`

``` purescript
onbeforeunload :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onerror`

``` purescript
onerror :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onhashchange`

``` purescript
onhashchange :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onload`

``` purescript
onload :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onpageshow`

``` purescript
onpageshow :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onpagehide`

``` purescript
onpagehide :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onresize`

``` purescript
onresize :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onscroll`

``` purescript
onscroll :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onunload`

``` purescript
onunload :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onchange`

``` purescript
onchange :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `oninput`

``` purescript
oninput :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `oninvalid`

``` purescript
oninvalid :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onreset`

``` purescript
onreset :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onsearch`

``` purescript
onsearch :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onselect`

``` purescript
onselect :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onsubmit`

``` purescript
onsubmit :: forall i. (Event () -> EventHandler i) -> H.Attr i
```


#### `onclick`

``` purescript
onclick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
```


#### `oncontextmenu`

``` purescript
oncontextmenu :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
```


#### `ondblclick`

``` purescript
ondblclick :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
```


#### `onmousedown`

``` purescript
onmousedown :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
```


#### `onmouseenter`

``` purescript
onmouseenter :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
```


#### `onmouseleave`

``` purescript
onmouseleave :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
```


#### `onmousemove`

``` purescript
onmousemove :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
```


#### `onmouseover`

``` purescript
onmouseover :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
```


#### `onmouseout`

``` purescript
onmouseout :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
```


#### `onmouseup`

``` purescript
onmouseup :: forall i. (Event MouseEvent -> EventHandler i) -> H.Attr i
```


#### `onkeydown`

``` purescript
onkeydown :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attr i
```


#### `onkeypress`

``` purescript
onkeypress :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attr i
```


#### `onkeyup`

``` purescript
onkeyup :: forall i. (Event KeyboardEvent -> EventHandler i) -> H.Attr i
```


#### `onblur`

``` purescript
onblur :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
```


#### `onfocus`

``` purescript
onfocus :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
```


#### `onfocusin`

``` purescript
onfocusin :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
```


#### `onfocusout`

``` purescript
onfocusout :: forall i. (Event FocusEvent -> EventHandler i) -> H.Attr i
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



