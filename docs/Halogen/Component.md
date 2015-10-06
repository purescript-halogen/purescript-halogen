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
- `p` - the type of slots within the component, used to specify locations
        at which child components can be installed.

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

#### `EvalP`

``` purescript
type EvalP i s s' f f' g p p' = Eval i s f (QueryF s s' f f' g p p')
```

A convenience variation on `Eval` for parent components.

#### `Peek`

``` purescript
type Peek s s' f f' g p p' = PeekP s f (QueryF s s' f f' g p p') (ChildF p f')
```

A type alias for a component `peek` function that observes inputs to child
components.

#### `PeekP`

``` purescript
type PeekP s f g o = forall a. o a -> Free (HalogenF s f g) Unit
```

A lower level form of the `Peek` type synonym, used internally.

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

#### `component`

``` purescript
component :: forall s f g p. Render s f p -> Eval f s f g -> Component s f g p
```

Builds a new [`Component`](#component) from a [`Render`](#render) and
[`Eval`](#eval) function.

#### `component'`

``` purescript
component' :: forall s s' f f' g p p'. Render s f p -> EvalP f s s' f f' g p p' -> Peek s s' f f' g p p' -> ParentComponentP s s' f f' g p p'
```

Builds a new [`ComponentP`](#componentp) from a [`Render`](#render),
[`Eval`](#eval), and [`Peek`](#peek) function. This is used in cases where
defining a parent component that needs to observe inputs to its children.

#### `ChildState`

``` purescript
type ChildState s f g p = Tuple (Component s f g p) s
```

A type synonym for a component combined with its state. This is used when
installing components into slots.

#### `createChild`

``` purescript
createChild :: forall s f g p. Component s f g p -> s -> ChildState s f g p
```

Creates a `ChildState` for a component.

#### `createChild'`

``` purescript
createChild' :: forall s s' f f' g p p' q. (Functor g) => ChildPath s s' f f' p p' -> Component s f g q -> s -> ChildState s' f' g q
```

Creates a `ChildState` for a component that is being installed into a
parent with multiple different types of child component.

#### `ParentComponent`

``` purescript
type ParentComponent s s' f f' g p p' = Component s f (QueryF s s' f f' g p p') p
```

A type alias used to simplify the type signature for a `Component s f g p`
that is intended to have components of type `Component s' f' g p'`
installed into it.

#### `ParentComponentP`

``` purescript
type ParentComponentP s s' f f' g p p' = ComponentP s f (QueryF s s' f f' g p p') (ChildF p f') p
```

A type alias similar to `ParentComponent`, but for components that `peek`
on their children.

#### `InstalledComponent`

``` purescript
type InstalledComponent s s' f f' g p p' = Component (InstalledState s s' f f' g p p') (Coproduct f (ChildF p f')) g p'
```

A type alias use to simplify the type signature for a `Component s f g p`
that has had components of type `Component s' f' g p'` installed into it.

#### `InstalledState`

``` purescript
type InstalledState s s' f f' g p p' = { parent :: s, children :: Map p (ChildState s' f' g p'), memo :: Map p (HTML p' (Coproduct f (ChildF p f') Unit)) }
```

The type used by component containers for their state where `s` is the
state local to the container, `p` is the type of slot used by the
container, and the remaining parameters are the type variables for the
child components.

#### `installedState`

``` purescript
installedState :: forall s s' f f' g p p'. (Ord p) => s -> InstalledState s s' f f' g p p'
```

Creates an initial `InstalledState` value for a component container based
on a state value for the container.

#### `QueryF`

``` purescript
type QueryF s s' f f' g p p' = Free (HalogenF (InstalledState s s' f f' g p p') (ChildF p f') g)
```

An intermediate algebra that parent components "produce" from their `eval`
and `peek` functions. This takes the place of `g` when compared to a leaf
(non-parent) component.

#### `ChildF`

``` purescript
data ChildF p f i
  = ChildF p (f i)
```

An intermediate algebra used to associate values from a child component's
algebra with the slot the component was installed into.

##### Instances
``` purescript
instance functorChildF :: (Functor f) => Functor (ChildF p f)
```

#### `query`

``` purescript
query :: forall s s' f f' g p p' i. (Functor g, Ord p) => p -> f' i -> Free (HalogenF s f (QueryF s s' f f' g p p')) (Maybe i)
```

Queries a child component, for use within a parent component's `eval` or
`peek` function.

#### `query'`

``` purescript
query' :: forall s s' s'' f f' f'' g p p' p'' i. (Functor g, Ord p') => ChildPath s s' f f' p p' -> p -> f i -> Free (HalogenF s'' f'' (QueryF s'' s' f'' f' g p' p'')) (Maybe i)
```

A version of [`query`](#query) for use when a parent component has multiple
types of child component.

#### `mkQuery`

``` purescript
mkQuery :: forall s s' f f' p p' g i. (Functor g, Ord p) => p -> f' i -> QueryF s s' f f' g p p' (Maybe i)
```

Creates a query for a child component where `p` is the slot the component
was installed into and `f' i` in the input query.

If a component is not found for the slot the result of the query
will be `Nothing`.

#### `mkQuery'`

``` purescript
mkQuery' :: forall s s' s'' f f' f'' g p p' p'' i. (Functor g, Ord p') => ChildPath s s' f f' p p' -> p -> f i -> QueryF s'' s' f'' f' g p' p'' (Maybe i)
```

A version of [`mkQuery`](#mkQuery) for use when a parent component has
multiple types of child component.

#### `liftQuery`

``` purescript
liftQuery :: forall s s' f f' g p p'. (Functor g) => EvalP (QueryF s s' f f' g p p') s s' f f' g p p'
```

Lifts a value in the `QueryF` algebra into the monad used by a component's
`eval` function.

#### `install`

``` purescript
install :: forall s s' f f' g p p'. (Plus g, Ord p) => ParentComponent s s' f f' g p p' -> (p -> ChildState s' f' g p') -> InstalledComponent s s' f f' g p p'
```

Installs children into a parent component by using a function that produces
`ChildState` values for a given slot.

#### `installWithState`

``` purescript
installWithState :: forall s s' f f' g p p'. (Plus g, Ord p) => ParentComponent s s' f f' g p p' -> (s -> p -> ChildState s' f' g p') -> InstalledComponent s s' f f' g p p'
```

A version of [`install`](#install) that gives us access to the parent's
state while installing children.

#### `install'`

``` purescript
install' :: forall s s' f f' g p p'. (Plus g, Ord p) => ParentComponentP s s' f f' g p p' -> (p -> ChildState s' f' g p') -> InstalledComponent s s' f f' g p p'
```

A version of [`install`](#install) for use with parent components that
`peek` on their children.

#### `installWithState'`

``` purescript
installWithState' :: forall s s' f f' g p p'. (Plus g, Ord p) => ParentComponentP s s' f f' g p p' -> (s -> p -> ChildState s' f' g p') -> InstalledComponent s s' f f' g p p'
```

A version of [`install'`](#install') that gives us access to the parent's
state while installing children.

#### `interpret`

``` purescript
interpret :: forall s f g g' p. (Functor g') => Natural g g' -> Component s f g p -> Component s f g' p
```

Changes the component's `g` type. A use case for this would be to interpret
some `Free` monad as `Aff` so the component can be used with `runUI`.


