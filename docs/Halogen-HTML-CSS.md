## Module Halogen.HTML.CSS

This module defines an adapter between the `purescript-halogen` and `purescript-css` libraries.

#### `Styles`

``` purescript
newtype Styles
  = Styles (StrMap String)
```

A newtype for CSS styles

##### Instances
``` purescript
instance stylesIsAttribute :: IsAttribute Styles
```

#### `runStyles`

``` purescript
runStyles :: Styles -> StrMap String
```

Unpack CSS styles

#### `style`

``` purescript
style :: forall i. Css -> Attr i
```

Render a set of rules as an inline style.

For example:

```purescript
H.div [ Css.style do color red
                     display block ]
      [ ... ]
```

#### `stylesheet`

``` purescript
stylesheet :: forall i. Css -> HTML i
```

Render a set of rules as a `style` element.


