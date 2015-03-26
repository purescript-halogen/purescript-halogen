# Module Documentation

## Module Halogen.HTML.Traversals


This module defines an initial encoding of the `HTML` type,
which can be used to implement traversals.

#### `Attr`

``` purescript
data Attr i
  = Attr (forall r. (forall value. (A.IsAttribute value) => A.AttributeName value -> value -> r) -> r)
  | Handler (forall r. (forall fields. A.EventName fields -> (Event fields -> EventHandler (Maybe i)) -> r) -> r)
```

A single attribute is either

- An attribute
- An event handler

Both are encoded as existentials-as-universals.

#### `HTML`

``` purescript
data HTML a i
  = Text String
  | Element H.TagName [Attr i] [HTML a i]
  | Placeholder a
```

An initial encoding of HTML nodes.

#### `toAttr`

``` purescript
toAttr :: forall i. A.Attr i -> Attr i
```

Convert the final encoding to the initial encoding.

#### `fromAttr`

``` purescript
fromAttr :: forall i. Attr i -> A.Attr i
```

Convert the initial encoding to the final encoding.

#### `toHTML`

``` purescript
toHTML :: forall p i. (forall node. (H.HTMLRepr node) => node p i) -> HTML p i
```

Convert the final encoding to the initial encoding.

#### `fromHTML`

``` purescript
fromHTML :: forall p i node. (H.HTMLRepr node) => HTML p i -> node p i
```

Convert the initial encoding to the final encoding.

#### `graft`

``` purescript
graft :: forall a b i. HTML a i -> (a -> HTML b i) -> HTML b i
```

Replace placeholder nodes with HTML documents.

#### `modify`

``` purescript
modify :: forall p q i j node. (H.HTMLRepr node) => (HTML p i -> HTML q j) -> (forall node. (H.HTMLRepr node) => node p i) -> node q j
```

Modify a HTML structure by using the intermediate representation presented in
this module.

#### `functorAttr`

``` purescript
instance functorAttr :: Functor Attr
```


#### `attrRepr`

``` purescript
instance attrRepr :: A.AttrRepr Attr
```


#### `bifunctorHTML`

``` purescript
instance bifunctorHTML :: Bifunctor HTML
```


#### `htmlRepr`

``` purescript
instance htmlRepr :: H.HTMLRepr HTML
```




