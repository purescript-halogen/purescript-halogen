# Module Documentation

## Module Halogen.HTML.Renderer.VirtualDOM

#### `renderHTML`

``` purescript
renderHTML :: forall p i eff. (i -> Eff eff Unit) -> (p -> Widget eff i) -> H.HTML p i -> VTree
```

Render a `HTML` document to a virtual DOM node

The first argument is an event handler.
The second argument is used to replace placeholder nodes.


## Module Halogen.HTML.Renderer.String

#### `renderHTMLToString`

``` purescript
renderHTMLToString :: forall i. H.HTML Void i -> String
```

Render a HTML document as a `String`, usually for testing purposes.



