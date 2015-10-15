## Module Halogen.HTML.Properties.Indexed.Unsafe

#### `IProp`

``` purescript
newtype IProp (r :: # *
) i
  = IProp (Prop i)
```

The phantom row `r` can be thought of as a context which is synthesized in the
course of constructing a refined HTML expression.


