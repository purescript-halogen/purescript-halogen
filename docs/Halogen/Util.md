## Module Halogen.Util

#### `appendTo`

``` purescript
appendTo :: forall m eff. (MonadEff (dom :: DOM | eff) m) => String -> HTMLElement -> m Unit
```

A utility for appending an `HTMLElement` to the element selected using querySelector
element once the document has loaded.

#### `appendToBody`

``` purescript
appendToBody :: forall m eff. (MonadEff (dom :: DOM | eff) m) => HTMLElement -> m Unit
```

A utility for appending an `HTMLElement` to the current document's `body`
element once the document has loaded.


