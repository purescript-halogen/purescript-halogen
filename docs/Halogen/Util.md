## Module Halogen.Util

#### `appendTo`

``` purescript
appendTo :: forall m eff. (MonadEff (dom :: DOM | eff) m) => String -> HTMLElement -> m Unit
```

A utility for appending an `HTMLElement` to the element selected using querySelector
element (synchronously).

#### `appendToBody`

``` purescript
appendToBody :: forall m eff. (MonadEff (dom :: DOM | eff) m) => HTMLElement -> m Unit
```

A utility for appending an `HTMLElement` to the current document's `body`
element once the document has loaded.

#### `onLoad`

``` purescript
onLoad :: forall m eff. (MonadEff (dom :: DOM | eff) m) => Eff (dom :: DOM | eff) Unit -> m Unit
```

On load, discard the onLoad event and call a synchronous action.


