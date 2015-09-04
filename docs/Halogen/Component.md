## Module Halogen.Component

#### `ComponentP`

``` purescript
newtype ComponentP s f g o p
```

Data type for Halogen components.
- `s` - the type of the component state
- `f` - the component's query algebra
- `g` - the monad handling the component's non-state effects
- `o` - the type of values observable via `peek`, used to allow parent
        components to see queries their children have acted upon.
- `p` - the type of placeholders within the component, used to specify
        "holes" in which child components can be installed.

##### Instances
``` purescript
instance functorComponent :: Functor (ComponentP s f g o)
```

#### `Component`

``` purescript
type Component s f g = ComponentP s f g (Const Void)
```

A type alias for Halogen components where `peek` is not used.

#### `Render`

``` purescript
type Render s f p = s -> HTML p (f Unit)
```

A type alias for a component `render` function.

#### `Eval`

``` purescript
type Eval i s f g = Natural i (Free (HalogenF s f g))
```

A type alias for a component `eval` function that takes a value from the
component's query algebra and returns a `Free` monad of the Halogen
component algebra.

#### `Peek`

``` purescript
type Peek s f g o = forall a. o a -> Free (HalogenF s f g) Unit
```

A type alias for a component `peek` function that observes inputs to child
components.

#### `renderComponent`

``` purescript
renderComponent :: forall s f g o p. ComponentP s f g o p -> s -> Tuple (HTML p (f Unit)) s
```

Runs a component's `render` function with the specified state, returning
the generated `HTML` and new state.

#### `queryComponent`

``` purescript
queryComponent :: forall s f g o p. ComponentP s f g o p -> Eval f s f g
```

Runs a compnent's `query` function with the specified query input and
returns the pending computation as a `Free` monad.

#### `component'`

``` purescript
component' :: forall s f g o p. Render s f p -> Eval f s f g -> Peek s f g o -> ComponentP s f g o p
```

Builds a new [`ComponentP`](#componentp) from a [`Render`](#render),
[`Eval`](#eval), and [`Peek`](#peek) function. This is used in cases where
defining a parent component that needs to observe inputs to its children.

#### `component`

``` purescript
component :: forall s f g p. Render s f p -> Eval f s f g -> Component s f g p
```

Builds a new [`Component`](#component) from a [`Render`](#render) and
[`Eval`](#eval) function.

#### `liftEff'`

``` purescript
liftEff' :: forall eff a s f g. (MonadEff eff g, Functor g) => Eff eff a -> Free (HalogenF s f g) a
```

Interacting with the DOM will usually be done via `Eff`, but we tend to
operate in `Aff` when using Halogen, so this function helps lift an `Eff`
action into an `Aff` or any other `MonadEff`-instance-providing-type.

This is identical to `liftFI <<< liftEff`, but typed in such a way that
code using `liftEff'` won't require decorating its usage with explicit
type signatures.

#### `ComponentStateP`

``` purescript
type ComponentStateP s f g o p = Tuple (ComponentP s f g o p) s
```

A type synonym for a component combined with its state. Used when
installing components to a component with initial state for a placeholder.

#### `ComponentState`

``` purescript
type ComponentState s f g p = ComponentStateP s f g (Const Void) p
```

#### `createChild`

``` purescript
createChild :: forall s s' f f' g o p p'. (Functor g) => InjectC s s' f f' p p' -> ComponentP s f g o p -> s -> ComponentStateP s' f' g o p
```

#### `InstalledStateP`

``` purescript
type InstalledStateP s s' f' g o' p p' = { parent :: s, children :: Map p (ComponentStateP s' f' g o' p') }
```

The type used by component containers for their state where `s` is the
state local to the container, `p` is the type of placeholder used by the
container, and the remaining parameters are the type variables for the
child components.

#### `InstalledState`

``` purescript
type InstalledState s s' f g p p' = InstalledStateP s s' f g (Const Void) p p'
```

#### `ParentComponentP`

``` purescript
type ParentComponentP s s' f f' g o o' p p' = ComponentP s f (QueryFP s s' f' g o' p p') o p
```

A type alias used to simplify the type signature for a `Component s f g p`
that is intended to have components of type `Component s' f' g p'`
installed into it.

#### `ParentComponent`

``` purescript
type ParentComponent s s' f f' g o' p p' = ParentComponentP s s' f f' g (Const Void) o' p p'
```

#### `InstalledComponentP`

``` purescript
type InstalledComponentP s s' f f' g o o' p p' = ComponentP (InstalledStateP s s' f' g o' p p') (Coproduct f (ChildF p f')) g o p'
```

A type alias use to simplify the type signature for a `Component s f g p`
that has had components of type `Component s' f' g p'` installed into it.

#### `InstalledComponent`

``` purescript
type InstalledComponent s s' f f' g o' p p' = InstalledComponentP s s' f f' g (Const Void) o' p p'
```

#### `installedState`

``` purescript
installedState :: forall s s' f' g o' p p'. (Ord p) => s -> InstalledStateP s s' f' g o' p p'
```

Creates an initial `InstalledState` value for a component container based
on a state value for the container.

#### `QueryFP`

``` purescript
type QueryFP s s' f' g o' p p' = Free (HalogenF (InstalledStateP s s' f' g o' p p') (ChildF p f') g)
```

An intermediate algebra that component containers "produce" (use as their
`g` type variable).

#### `QueryF`

``` purescript
type QueryF s s' f' g p p' = QueryFP s s' f' g (Const Void) p p'
```

#### `ChildF`

``` purescript
data ChildF p f i
  = ChildF p (f i)
```

An intermediate algebra used to associate values from a child component's
algebra with the child component's placeholder when querying.

##### Instances
``` purescript
instance functorChildF :: (Functor f) => Functor (ChildF p f)
```

#### `mkQuery`

``` purescript
mkQuery :: forall s s' f' p p' o g i. (Functor g, Ord p) => p -> f' i -> QueryFP s s' f' g o p p' (Maybe i)
```

Creates a query for a child component where `p` is the placeholder
addressing the component and `f' i` in the input query.

If a component is not found for the placeholder the result of the query
will be `Nothing`.

#### `mkQuery'`

``` purescript
mkQuery' :: forall s s' s'' f f' g o p p' p'' i. (Functor g, Ord p') => InjectC s'' s' f f' p p' -> p -> f i -> QueryFP s s' f' g o p' p'' (Maybe i)
```

#### `liftQuery`

``` purescript
liftQuery :: forall s s' f f' g o' p p'. (Functor g) => Eval (QueryFP s s' f' g o' p p') s f (QueryFP s s' f' g o' p p')
```

Lifts a value in the `QueryF` algebra into the monad used by a component's
`eval` function.

#### `query`

``` purescript
query :: forall s s' f f' g o' p p' i. (Functor g, Ord p) => p -> f' i -> Free (HalogenF s f (QueryFP s s' f' g o' p p')) (Maybe i)
```

#### `query'`

``` purescript
query' :: forall s s' s'' f f' f'' g o' p p' p'' i. (Functor g, Ord p) => InjectC s'' s' f'' f' p'' p -> p'' -> f'' i -> Free (HalogenF s f (QueryFP s s' f' g o' p p')) (Maybe i)
```

#### `install`

``` purescript
install :: forall s s' f f' g o' p p'. (Plus g, Ord p) => ParentComponent s s' f f' g o' p p' -> (p -> ComponentStateP s' f' g o' p') -> InstalledComponent s s' f f' g o' p p'
```

#### `install'`

``` purescript
install' :: forall s s' f f' g o' p p'. (Plus g, Ord p) => ParentComponentP s s' f f' g (ChildF p f') o' p p' -> (p -> ComponentStateP s' f' g o' p') -> InstalledComponentP s s' f f' g (ChildF p f') o' p p'
```

#### `transform`

``` purescript
transform :: forall s s' f f' g o o' p p'. (Functor g) => (s -> s') -> (s' -> s) -> Natural f f' -> Natural f' f -> Natural o' o -> (p -> p') -> ComponentP s f g o p -> ComponentP s' f' g o' p'
```


