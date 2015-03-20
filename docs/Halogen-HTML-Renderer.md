# Module Documentation

## Module Halogen.HTML.Renderer.VirtualDOM

#### `functorAttr`

``` purescript
instance functorAttr :: Functor Attr
```


#### `altAttr`

``` purescript
instance altAttr :: Alt Attr
```


#### `plusAttr`

``` purescript
instance plusAttr :: Plus Attr
```


#### `attrRepr`

``` purescript
instance attrRepr :: H.AttrRepr Attr
```


#### `bifunctorHTML`

``` purescript
instance bifunctorHTML :: Bifunctor HTML
```


#### `htmlRepr`

``` purescript
instance htmlRepr :: H.HTMLRepr HTML
```


#### `renderHTML`

``` purescript
renderHTML :: forall p i eff. (i -> Eff eff Unit) -> (p -> VTree) -> H.HTML p i -> VTree
```

Render a `HTML` document to a virtual DOM node

The first argument is an event handler.
The second argument is used to replace placeholder nodes.


## Module Halogen.HTML.Renderer.String

#### `functorAttr`

``` purescript
instance functorAttr :: Functor Attr
```


#### `altAttr`

``` purescript
instance altAttr :: Alt Attr
```


#### `plusAttr`

``` purescript
instance plusAttr :: Plus Attr
```


#### `attrRepr`

``` purescript
instance attrRepr :: H.AttrRepr Attr
```


#### `bifunctorHTML`

``` purescript
instance bifunctorHTML :: Bifunctor HTML
```


#### `htmlRepr`

``` purescript
instance htmlRepr :: H.HTMLRepr HTML
```


#### `renderHTMLToString`

``` purescript
renderHTMLToString :: (forall p i. H.HTML p i) -> String
```

Render a HTML document as a `String`, usually for testing purposes.

The rank-2 type ensures that neither events nor placeholders are allowed.



