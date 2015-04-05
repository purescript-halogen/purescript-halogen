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
input :: forall i m a. (Applicative m) => (a -> i) -> a -> EventHandlerT m i
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

#### `handlerT`

``` purescript
handlerT :: forall fields m i. H.EventName fields -> (Event fields -> EventHandlerT m i) -> H.Attr (m i)
```

Attach an event handler which uses `EventHandlerT`.

#### `onabort`

``` purescript
onabort :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onbeforeunload`

``` purescript
onbeforeunload :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onerror`

``` purescript
onerror :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onhashchange`

``` purescript
onhashchange :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onload`

``` purescript
onload :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onpageshow`

``` purescript
onpageshow :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onpagehide`

``` purescript
onpagehide :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onresize`

``` purescript
onresize :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onscroll`

``` purescript
onscroll :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onunload`

``` purescript
onunload :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onchange`

``` purescript
onchange :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `oninput`

``` purescript
oninput :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `oninvalid`

``` purescript
oninvalid :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onreset`

``` purescript
onreset :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onsearch`

``` purescript
onsearch :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onselect`

``` purescript
onselect :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onsubmit`

``` purescript
onsubmit :: forall m i. (Event () -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onclick`

``` purescript
onclick :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `oncontextmenu`

``` purescript
oncontextmenu :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `ondblclick`

``` purescript
ondblclick :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onmousedown`

``` purescript
onmousedown :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onmouseenter`

``` purescript
onmouseenter :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onmouseleave`

``` purescript
onmouseleave :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onmousemove`

``` purescript
onmousemove :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onmouseover`

``` purescript
onmouseover :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onmouseout`

``` purescript
onmouseout :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onmouseup`

``` purescript
onmouseup :: forall m i. (Event MouseEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onkeydown`

``` purescript
onkeydown :: forall m i. (Event KeyboardEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onkeypress`

``` purescript
onkeypress :: forall m i. (Event KeyboardEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onkeyup`

``` purescript
onkeyup :: forall m i. (Event KeyboardEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onblur`

``` purescript
onblur :: forall m i. (Event FocusEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onfocus`

``` purescript
onfocus :: forall m i. (Event FocusEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onfocusin`

``` purescript
onfocusin :: forall m i. (Event FocusEvent -> EventHandlerT m i) -> H.Attr (m i)
```


#### `onfocusout`

``` purescript
onfocusout :: forall m i. (Event FocusEvent -> EventHandlerT m i) -> H.Attr (m i)
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



