## Module Halogen.Component

#### `Component`

``` purescript
newtype Component s f g p
```

Data type for Halogen components.
- `s` - the type of the component state
- `f` - the component's query algebra
- `g` - the monad handling the component's non-state effects
- `p` - the type of placeholders within the component, used to specify
        "holes" in which child components can be installed.

##### Instances
``` purescript
instance functorComponent :: Functor (Component s f g)
```

#### `ComponentF`

``` purescript
type ComponentF s f = Component s (Free f)
```

A type alias for a Halogen component using `Free` for its query algebra.

#### `ComponentFC`

``` purescript
type ComponentFC s f = Component s (FreeC f)
```

A type alias for a Halogen component using `FreeC` for their query algebra.
This removes the need to write an explicit `Functor` instance for `f`.

#### `Render`

``` purescript
type Render s p f = s -> HTML p (f Unit)
```

A type alias for a component `render` function.

#### `RenderF`

``` purescript
type RenderF s p f = s -> HTML p (Free f Unit)
```

A type alias for a component `render` function where the component is using
`Free` for its query algebra.

#### `RenderFC`

``` purescript
type RenderFC s p f = s -> HTML p (FreeC f Unit)
```

A type alias for a component `render` function where the component is using
`FreeC` for its query algebra.

#### `Eval`

``` purescript
type Eval f s g = Natural f (Free (Coproduct (StateF s) g))
```

A type alias for a component `query` function that a value from the
component's query algebra and returns a `Free` monad with state and `g`
effects.

#### `renderComponent`

``` purescript
renderComponent :: forall s f g p. Component s f g p -> s -> Tuple (HTML p (f Unit)) s
```

Runs a component's `render` function with the specified state, returning
the generated `HTML` and new state.

#### `queryComponent`

``` purescript
queryComponent :: forall s f g p i. Component s f g p -> f i -> Free (Coproduct (StateF s) g) i
```

Runs a compnent's `query` function with the specified query input and
returns the pending computation as a `Free` monad.

#### `component`

``` purescript
component :: forall s f g p. Render s p f -> Eval f s g -> Component s f g p
```

Builds a new [`Component`](#component) from a [`Render`](#render) and
[`Eval`](#eval) function.

#### `componentF`

``` purescript
componentF :: forall s f g p. (Functor f, Functor g) => RenderF s p f -> Eval f s g -> ComponentF s f g p
```

Builds a new [`ComponentF`](#componentf) from a [`RenderF`](#renderf) and
[`Eval`](#eval) function.

#### `componentFC`

``` purescript
componentFC :: forall s f g p. (Functor g) => RenderFC s p f -> Eval f s g -> ComponentFC s f g p
```

Builds a new [`ComponentFC`](#componentfc) from a [`RenderFC`](#renderfc)
and [`Eval`](#eval) function.

#### `ComponentState`

``` purescript
type ComponentState s f g p = Tuple s (Component s f g p)
```

#### `InstalledState`

``` purescript
type InstalledState s s' f' p p' g = { parent :: s, children :: Map p (ComponentState s' f' g p') }
```

#### `ChildF`

``` purescript
data ChildF p f i
```

##### Instances
``` purescript
instance functorChildF :: (Functor f) => Functor (ChildF p f)
```

#### `installL`

``` purescript
installL :: forall s f g pl pr s' f' p'. (Ord pl, Monad g, Plus g) => Component s f (QueryF s s' f' pl p' g) (Either pl pr) -> (pl -> ComponentState s' f' g p') -> Component (InstalledState s s' f' pl p' g) (Coproduct f (ChildF pl f')) g (Either p' pr)
```

#### `installR`

``` purescript
installR :: forall s f g pl pr s' f' p'. (Ord pr, Monad g, Plus g) => Component s f (QueryF s s' f' pr p' g) (Either pl pr) -> (pr -> ComponentState s' f' g p') -> Component (InstalledState s s' f' pr p' g) (Coproduct f (ChildF pr f')) g (Either pl p')
```

#### `installAll`

``` purescript
installAll :: forall s f g p s' f' p'. (Ord p, Monad g, Plus g) => Component s f (QueryF s s' f' p p' g) p -> (p -> ComponentState s' f' g p') -> Component (InstalledState s s' f' p p' g) (Coproduct f (ChildF p f')) g p'
```

#### `QueryF`

``` purescript
type QueryF s s' f' p p' g = Free (Coproduct (StateF (InstalledState s s' f' p p' g)) g)
```

#### `query`

``` purescript
query :: forall s s' f' p p' g i. (Functor g, Ord p) => p -> f' i -> QueryF s s' f' p p' g (Maybe i)
```


