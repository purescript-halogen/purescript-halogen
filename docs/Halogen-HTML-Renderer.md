## Module Halogen.HTML.Renderer.VirtualDOM

#### `renderHTML`

``` purescript
renderHTML :: forall i eff. (i -> Eff eff Unit) -> HTML i -> VTree
```

Render a `HTML` document to a virtual DOM node

The first argument is an event handler.


## Module Halogen.HTML.Renderer.String

#### `renderHTMLToString`

``` purescript
renderHTMLToString :: forall i. HTML i -> String
```

Render a HTML document as a `String`, usually for testing purposes.


