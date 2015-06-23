## Module Halogen.HTML.Renderer.VirtualDOM

#### `RenderState`

``` purescript
type RenderState = { initializers :: StrMap Props, finalizers :: StrMap Props }
```

#### `emptyRenderState`

``` purescript
emptyRenderState :: RenderState
```

#### `renderHTML`

``` purescript
renderHTML :: forall p f eff. (forall i. f i -> Aff (HalogenEffects eff) i) -> HTML p (f Unit) -> RenderState -> Tuple VTree RenderState
```

Render a `HTML` document to a virtual DOM node

The first argument is an event handler.


