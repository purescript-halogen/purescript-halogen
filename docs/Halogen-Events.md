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

#### `input`

``` purescript
input :: forall i m a. (Applicative m) => (a -> i) -> a -> EventHandler (m i)
```

A helper function which can be used to create simple event handlers.

Often we don't need to use `EventHandler` or the monad underlying our component, and just need
to generate an input to the signal function. 

This function provides an alternative to making two nested calls to `pure`:

```purescript
onclick (input \_ -> Input)
```

#### `withEventHandlerT`

``` purescript
withEventHandlerT :: forall i m e. (e -> EventHandlerT m i) -> e -> EventHandler (m i)
```

Create an event handler which uses `EventHandlerT`.

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

#### `EventHandlerT`

``` purescript
data EventHandlerT m a
```

This `Applicative` transformer supports the following operations on events:

- `preventDefault`
- `stopPropagation`
- `stopImmediatePropagation`
- `cancel`

It can be used as follows:

```purescript
import Control.Functor (($>))

H.a (E.onclick \_ -> E.preventDefault $> ClickHandler) (H.text "Click here")
```

#### `EventHandler`

``` purescript
type EventHandler = EventHandlerT Identity
```

`EventHandler` is a synonym for `EventHandlerT` applied to the `Identity` monad.

That is, `EventHandler` only adds the `preventDefault`, `stopPropagation`,
`stopImmediatePropagation` and `cancel` actions to the underlying monad.

#### `preventDefault`

``` purescript
preventDefault :: forall m. (Applicative m) => EventHandlerT m Unit
```

Call the `preventDefault` method on the current event

#### `stopPropagation`

``` purescript
stopPropagation :: forall m. (Applicative m) => EventHandlerT m Unit
```

Call the `stopPropagation` method on the current event

#### `stopImmediatePropagation`

``` purescript
stopImmediatePropagation :: forall m. (Applicative m) => EventHandlerT m Unit
```

Call the `stopImmediatePropagation` method on the current event

#### `cancel`

``` purescript
cancel :: forall m a. EventHandlerT m a
```

Cancel the event, so that no input data will be passed to the signal function

#### `liftEventHandler`

``` purescript
liftEventHandler :: forall m a. m a -> EventHandlerT m a
```

Lift an action into the `EventHandlerT` transformer

#### `unwrapEventHandler`

``` purescript
unwrapEventHandler :: forall m a. EventHandlerT m a -> EventHandler (m a)
```

Interpret `EventHandlerT` in terms of `EventHandler`.

#### `functorEventHandler`

``` purescript
instance functorEventHandler :: (Functor m) => Functor (EventHandlerT m)
```


#### `applyEventHandler`

``` purescript
instance applyEventHandler :: (Apply m) => Apply (EventHandlerT m)
```


#### `applicativeEventHandler`

``` purescript
instance applicativeEventHandler :: (Applicative m) => Applicative (EventHandlerT m)
```


#### `runEventHandler`

``` purescript
runEventHandler :: forall a fields eff. Event fields -> EventHandler a -> Eff (dom :: DOM | eff) (Maybe a)
```

This function can be used to update an event and return the wrapped value



