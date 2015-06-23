## Module Halogen.HTML.Renderer.VirtualDOM

#### `renderHTML`

``` purescript
renderHTML :: forall p i eff. (i -> Eff eff Unit) -> HTML p i -> VTree
```

Render a `HTML` document to a virtual DOM node

The first argument is an event handler.


## Module Halogen.HTML.Renderer.String

#### `renderHTMLToString`

``` purescript
renderHTMLToString :: forall p i. HTML p i -> String
```

Render a HTML document as a `String`, usually for testing purposes.


