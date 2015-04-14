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
newtype EventHandler a
```

This monad supports the following operations on events:

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


#### `bindEventHandler`

``` purescript
instance bindEventHandler :: Bind EventHandler
```


#### `monadEventHandler`

``` purescript
instance monadEventHandler :: Monad EventHandler
```


#### `runEventHandler`

``` purescript
runEventHandler :: forall a fields eff. Event fields -> EventHandler a -> Eff (dom :: DOM | eff) a
```

This function can be used to update an event and return the wrapped value


## Module Halogen.HTML.Events.Monad


This module defines the `Event` monad.

#### `Event`

``` purescript
newtype Event eff a
  = Event (ListT (Aff eff) a)
```

The `Event` monad, which supports the asynchronous generation of events.

This monad is used in the definition of `runUI`.

#### `unEvent`

``` purescript
unEvent :: forall eff a. Event eff a -> ListT (Aff eff) a
```

Unwrap the `Event` constructor.

#### `runEvent`

``` purescript
runEvent :: forall eff a. (Error -> Eff eff Unit) -> (a -> Eff eff Unit) -> Event eff a -> Eff eff Unit
```

Run a computation in the `Event` monad by providing a callback function.

The callback function will be invoked zero or more times.

#### `yield`

``` purescript
yield :: forall eff a. a -> Event eff a
```

Yield an event. In practice, the event will be passed to the driver function.

#### `async`

``` purescript
async :: forall eff a. Aff eff a -> Event eff a
```

Lift an asynchronous computation into the `Event` monad.

#### `andThen`

``` purescript
andThen :: forall eff a. Event eff a -> (a -> Event eff a) -> Event eff a
```

A combinator which branches based on the supplied function after the first result,
and returns to the original stream of events after the secondary stream has been
exhausted.

#### `semigroupEvent`

``` purescript
instance semigroupEvent :: Semigroup (Event eff a)
```


#### `monoidEvent`

``` purescript
instance monoidEvent :: Monoid (Event eff a)
```


#### `functorEvent`

``` purescript
instance functorEvent :: Functor (Event eff)
```


#### `applyEvent`

``` purescript
instance applyEvent :: Apply (Event eff)
```


#### `applicativeEvent`

``` purescript
instance applicativeEvent :: Applicative (Event eff)
```


#### `bindEvent`

``` purescript
instance bindEvent :: Bind (Event eff)
```


#### `monadEvent`

``` purescript
instance monadEvent :: Monad (Event eff)
```


#### `monadEffEvent`

``` purescript
instance monadEffEvent :: MonadEff eff (Event eff)
```


#### `monadAffEvent`

``` purescript
instance monadAffEvent :: MonadAff eff (Event eff)
```


#### `altEvent`

``` purescript
instance altEvent :: Alt (Event eff)
```


#### `plusEvent`

``` purescript
instance plusEvent :: Plus (Event eff)
```


#### `alternativeEvent`

``` purescript
instance alternativeEvent :: Alternative (Event eff)
```


#### `monadPlusEvent`

``` purescript
instance monadPlusEvent :: MonadPlus (Event eff)
```




