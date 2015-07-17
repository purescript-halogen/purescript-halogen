## Module Halogen.HTML.Renderer.VirtualDOM

#### `renderHTML`

``` purescript
renderHTML :: forall p f eff. (forall i. f i -> Aff (HalogenEffects eff) i) -> HTML p (f Unit) -> VTree
```

Render a `HTML` document to a virtual DOM node

The first argument is an event handler.


