## Module Halogen.Component

#### `Component`

``` purescript
newtype Component s f g
```

Data type for Halogen components.
- `s` - the component's state
- `f` - the component's query algebra
- `g` - a functor integrated into the component's query algebra that allows
        embedding of external DSLs or handling of effects.

#### `ComponentHTML`

``` purescript
type ComponentHTML f = HTML Void (f Unit)
```

The type for `HTML` rendered by a self-contained component.

#### `Render`

``` purescript
type Render s f = s -> ComponentHTML f
```

A type alias for a component `render` function - takes the component's
current state and returns a `HTML` value.

#### `ComponentDSL`

``` purescript
type ComponentDSL s f g = Free (HalogenF s f g)
```

The DSL used in the `eval` function for self-contained components.

#### `Eval`

``` purescript
type Eval i s f g = Natural i (ComponentDSL s f g)
```

A type alias for a component `eval` function - takes a functorial value `i`
and returns a `Free` of the Halogen component algebra.

Usually `i` will be the same type as `f`, but sometimes it is useful to be
able to break up an `Eval` function into different parts, in which case
`i`'s type may differ.

#### `component`

``` purescript
component :: forall s f g. Render s f -> Eval f s f g -> Component s f g
```

Builds a self-contained component with no possible children.

#### `ParentHTML`

``` purescript
type ParentHTML s' f f' g p = HTML (SlotConstructor s' f' g p) (f Unit)
```

The type for `HTML` rendered by a parent component.

#### `RenderParent`

``` purescript
type RenderParent s s' f f' g p = s -> ParentHTML s' f f' g p
```

A variation on `Render` for parent components - the function follows the
same form but the type representation is different.

#### `SlotConstructor`

``` purescript
data SlotConstructor s' f' g p
  = SlotConstructor p (Unit -> { component :: Component s' f' g, initialState :: s' })
```

The type used for slots in the HTML rendered by parent components.

#### `ParentDSL`

``` purescript
type ParentDSL s s' f f' g p = Free (HalogenFP ParentEventSource s f (QueryF s s' f f' g p))
```

The DSL used in the `eval` and `peek` functions for parent components.

#### `EvalParent`

``` purescript
type EvalParent i s s' f f' g p = Natural i (ParentDSL s s' f f' g p)
```

A variation on `Eval` for parent components - the function follows the
same form but the type representation is different.

#### `Peek`

``` purescript
type Peek i s s' f f' g p = forall a. i a -> ParentDSL s s' f f' g p Unit
```

A type alias for a component `peek` function that observes inputs to child
components.

#### `parentComponent`

``` purescript
parentComponent :: forall s s' f f' g p. (Functor g, Ord p) => RenderParent s s' f f' g p -> EvalParent f s s' f f' g p -> Component (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
```

Builds a component that may contain child components.

#### `parentComponent'`

``` purescript
parentComponent' :: forall s s' f f' g p. (Functor g, Ord p) => RenderParent s s' f f' g p -> EvalParent f s s' f f' g p -> Peek (ChildF p f') s s' f f' g p -> Component (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
```

Builds a component that may contain child components and additionally
supports the `peek` operation to allow the parent to observe queries that
descendant components have processed.

#### `InstalledState`

``` purescript
newtype InstalledState s s' f f' g p
```

The type used by component containers for their state where `s` is the
state local to the container, `p` is the type of slot used by the
container, and the remaining parameters are the type variables for the
child components.

#### `installedState`

``` purescript
installedState :: forall s s' f f' g p. (Ord p) => s -> InstalledState s s' f f' g p
```

Lifts a state value into an `InstalledState` value. Useful when providing
an initial state value for a parent component.

#### `QueryF`

``` purescript
type QueryF s s' f f' g p = Free (HalogenF (InstalledState s s' f f' g p) (ChildF p f') g)
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
query :: forall s s' f f' g p i. (Functor g, Ord p) => p -> f' i -> Free (HalogenFP ParentEventSource s f (QueryF s s' f f' g p)) (Maybe i)
```

Queries a child component, for use within a parent component's `eval` or
`peek` function.

#### `query'`

``` purescript
query' :: forall s s' s'' f f' f'' g p p' i. (Functor g, Ord p') => ChildPath s s' f f' p p' -> p -> f i -> Free (HalogenFP ParentEventSource s'' f'' (QueryF s'' s' f'' f' g p')) (Maybe i)
```

A version of [`query`](#query) for use when a parent component has multiple
types of child component.

#### `mkQuery`

``` purescript
mkQuery :: forall s s' f f' p g i. (Functor g, Ord p) => p -> f' i -> QueryF s s' f f' g p (Maybe i)
```

Creates a query for a child component where `p` is the slot the component
was installed into and `f' i` in the input query.

If a component is not found for the slot the result of the query
will be `Nothing`.

#### `mkQuery'`

``` purescript
mkQuery' :: forall s s' s'' f f' f'' g p p' i. (Functor g, Ord p') => ChildPath s s' f f' p p' -> p -> f i -> QueryF s'' s' f'' f' g p' (Maybe i)
```

A version of [`mkQuery`](#mkQuery) for use when a parent component has
multiple types of child component.

#### `liftQuery`

``` purescript
liftQuery :: forall s s' f f' g p. (Functor g) => EvalParent (QueryF s s' f f' g p) s s' f f' g p
```

Lifts a value in the `QueryF` algebra into the monad used by a component's
`eval` function.

#### `transform`

``` purescript
transform :: forall s s' f f' g. (Functor g) => (s -> s') -> (s' -> Maybe s) -> (forall a. f a -> f' a) -> (forall a. f' a -> Maybe (f a)) -> Component s f g -> Component s' f' g
```

Transforms a `Component`'s types using partial mapping functions.

If the initial state provided to the component fails the transformation an
empty component will be rendered. If either of the transformations fail the
component will "halt" (evaluate to `empty`), so care must be taken when
handling transformed components to ensure they receive the intended query
values and initial state type.

Halogen itself will never cause a `transform`ed component to halt; this
situation will only arise when the initial state is incorrect or a bad
externally constructed query is passed to the component.

#### `transformChild`

``` purescript
transformChild :: forall s s' f f' g p p'. (Functor g) => ChildPath s s' f f' p p' -> Component s f g -> Component s' f' g
```

Transforms a `Component`'s types using a `ChildPath` definition.

#### `interpret`

``` purescript
interpret :: forall s f g g'. (Functor g') => Natural g g' -> Component s f g -> Component s f g'
```

Changes the component's `g` type. A use case for this would be to interpret
some `Free` monad as `Aff` so the component can be used with `runUI`.

#### `renderComponent`

``` purescript
renderComponent :: forall s f g. Component s f g -> s -> Tuple (HTML Void (f Unit)) s
```

Runs a component's `render` function with the specified state, returning
the generated `HTML` and new state.

#### `queryComponent`

``` purescript
queryComponent :: forall s f g. Component s f g -> Eval f s f g
```

Runs a compnent's `query` function with the specified query input and
returns the pending computation as a `Free` monad.


