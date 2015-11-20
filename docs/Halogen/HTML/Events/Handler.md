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
import Data.Functor (($>))

H.a [ E.onClick \_ -> E.preventDefault $> clickHandler ]
    [ H.text "Click here" ]
```

##### Instances
``` purescript
Functor EventHandler
Apply EventHandler
Applicative EventHandler
Bind EventHandler
Monad EventHandler
```

#### `preventDefault`

``` purescript
preventDefault :: EventHandler Unit
```

Call the `preventDefault` method on the current event.

#### `stopPropagation`

``` purescript
stopPropagation :: EventHandler Unit
```

Call the `stopPropagation` method on the current event.

#### `stopImmediatePropagation`

``` purescript
stopImmediatePropagation :: EventHandler Unit
```

Call the `stopImmediatePropagation` method on the current event.

#### `runEventHandler`

``` purescript
runEventHandler :: forall a fields m eff. (Monad m, MonadEff (dom :: DOM | eff) m) => Event fields -> EventHandler a -> m a
```

This function can be used to update an event and return the wrapped value


