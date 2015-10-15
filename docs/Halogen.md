## Module Halogen

The base Halogen module re-exports most of the library's useful types and
combinators, aside from the `HTML`-building functionality - the HTML
modules export a large number of commonly named values that are likely to
conflict.

#### `HTML`

``` purescript
type HTML p i = HTML p (i Unit)
```

A specialised version of the `Halogen.HTML.Core.HTML` type where `i` is
`* -> *` kinded to match the kind of a component query algebra.

#### `Prop`

``` purescript
type Prop i = Prop (i Unit)
```

A specialised version of the `Halogen.HTML.Core.Prop` type where `i` is
`* -> *` kinded to match the kind of a component query algebra.


