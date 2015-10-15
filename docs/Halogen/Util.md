## Module Halogen.Util

#### `appendToBody`

``` purescript
appendToBody :: forall m eff. (MonadEff (dom :: DOM | eff) m) => HTMLElement -> m Unit
```

A utility for appending an `HTMLElement` to the current document's `body`
element once the document has loaded.


