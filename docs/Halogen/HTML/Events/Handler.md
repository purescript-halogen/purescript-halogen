## Module Halogen.HTML.Events.Handler

The `EventHandler` monad, used to perform standard operations on HTML
events.

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

##### Instances
``` purescript
instance functorEventHandler :: Functor EventHandler
instance applyEventHandler :: Apply EventHandler
instance applicativeEventHandler :: Applicative EventHandler
instance bindEventHandler :: Bind EventHandler
instance monadEventHandler :: Monad EventHandler
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

#### `runEventHandler`

``` purescript
runEventHandler :: forall a fields eff. Event fields -> EventHandler a -> Eff (dom :: DOM | eff) a
```

This function can be used to update an event and return the wrapped value


