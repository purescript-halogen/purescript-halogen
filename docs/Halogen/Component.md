## Module Halogen.Component

#### `Component`

``` purescript
newtype Component s f g p
```

##### Instances
``` purescript
instance functorComponent :: Functor (Component s f g)
```

#### `ComponentF`

``` purescript
type ComponentF s f = Component s (Free f)
```

#### `ComponentFC`

``` purescript
type ComponentFC s f = Component s (FreeC f)
```

#### `Render`

``` purescript
type Render s p f = s -> HTML p (f Unit)
```

#### `RenderF`

``` purescript
type RenderF s p f = s -> HTML p (Free f Unit)
```

#### `RenderFC`

``` purescript
type RenderFC s p f = s -> HTML p (FreeC f Unit)
```

#### `Eval`

``` purescript
type Eval f s g = Natural f (Free (Coproduct (StateF s) g))
```

#### `renderComponent`

``` purescript
renderComponent :: forall s f g p. Component s f g p -> s -> Tuple (HTML p (f Unit)) s
```

#### `queryComponent`

``` purescript
queryComponent :: forall s f g p i. Component s f g p -> f i -> Free (Coproduct (StateF s) g) i
```

#### `component`

``` purescript
component :: forall s f g p. Render s p f -> Eval f s g -> Component s f g p
```

#### `componentF`

``` purescript
componentF :: forall s f g p. (Functor f, Functor g) => RenderF s p f -> Eval f s g -> ComponentF s f g p
```

#### `componentFC`

``` purescript
componentFC :: forall s f g p. (Functor g) => RenderFC s p f -> Eval f s g -> ComponentFC s f g p
```

#### `ComponentState`

``` purescript
type ComponentState s f g p = Tuple s (Component s f g p)
```

#### `InstalledState`

``` purescript
type InstalledState s s' f' p p' g = { parent :: s, children :: Map p (ComponentState s' f' g p') }
```


