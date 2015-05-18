# Module Documentation

## Module Halogen.HTML.CSS


This module defines an adapter between the `purescript-halogen` and `purescript-css` libraries.

#### `Styles`

``` purescript
newtype Styles
  = Styles (SM.StrMap String)
```

A newtype for CSS styles

#### `runStyles`

``` purescript
runStyles :: Styles -> SM.StrMap String
```

Unpack CSS styles

#### `stylesIsAttribute`

``` purescript
instance stylesIsAttribute :: A.IsAttribute Styles
```


#### `style`

``` purescript
style :: forall i. Css -> A.Attr i
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
stylesheet :: forall i. Css -> H.HTML i
```

Render a set of rules as a `style` element.



