# Module Documentation

## Module Halogen.HTML.Renderer.VirtualDOM

#### `Attr`

``` purescript
newtype Attr i
```


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


#### `HTML`

``` purescript
newtype HTML p i
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



