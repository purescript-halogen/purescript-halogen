## Module Halogen.Mixin.Router

This module provides helper functions for working with URL hashes.

#### `Hash`

``` purescript
newtype Hash
```

A type-safe wrapper for the hash component of a URL

#### `runHash`

``` purescript
runHash :: Hash -> String
```

Unwrap a `Hash` to get a `String`.

#### `onHashChange`

``` purescript
onHashChange :: forall i eff. (Hash -> i) -> Driver i eff -> Eff (HalogenEffects eff) Unit
```

Listen for hash change events, and provide an input to the driver function when one occurs.


