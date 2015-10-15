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
type Component s f g = ComponentP s f g (Const Void) Void
```

A type alias for self-contained Halogen components.

#### `RenderP`

``` purescript
type RenderP s f p = s -> HTML p (f Unit)
```

A low level form of the `Render` and `RenderParent` synonyms, used internally.

#### `Render`

``` purescript
type Render s f = RenderP s f Void
```

A type alias for a component `render` function.

#### `Eval`

``` purescript
type Eval i s f g = Natural i (Free (HalogenF s f g))
```

A type alias for a component `eval` function that takes a value from the
component's query algebra and returns a `Free` monad of the Halogen
component algebra.

#### `component`

``` purescript
component :: forall s f g. Render s f -> Eval f s f g -> Component s f g
```

Builds a self-contained component with no children.

#### `RenderParent`

``` purescript
type RenderParent s s' f f' g p = RenderP s f (SlotConstructor s' f' g p)
```

A variation on `Render` for parent components.

#### `SlotConstructor`

``` purescript
data SlotConstructor s' f' g p
  = SlotConstructor p (Component s' f' g) (Unit -> s')
```

The type used for slots in the HTML rendered by parent components.

#### `EvalParent`

``` purescript
type EvalParent i s s' f f' g p = Eval i s f (QueryF s s' f f' g p)
```

A variation on `Eval` for parent components.

#### `PeekP`

``` purescript
type PeekP i s f g = forall a. i a -> Free (HalogenF s f g) Unit
```

A low level form of the `Peek` type synonym, used internally.

#### `Peek`

``` purescript
type Peek i s s' f f' g p = PeekP i s f (QueryF s s' f f' g p)
```

A type alias for a component `peek` function that observes inputs to child
components.

#### `parentComponent`

``` purescript
parentComponent :: forall s s' f f' g p. (Plus g, Ord p) => RenderParent s s' f f' g p -> EvalParent f s s' f f' g p -> Component (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
```

Builds a component that can contain child components.

#### `parentComponent'`

``` purescript
parentComponent' :: forall s s' f f' g p. (Plus g, Ord p) => RenderParent s s' f f' g p -> EvalParent f s s' f f' g p -> Peek (ChildF p f') s s' f f' g p -> Component (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
```

Builds a component that can contain child components and supports the
`peek` operation to allow the parent to observe queries that descendant
components have processed.

#### `ParentComponent`

``` purescript
type ParentComponent s s' f f' g p = Component (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
```

#### `InstalledState`

``` purescript
type InstalledState s s' f f' g p = { parent :: s, children :: Map p (Tuple (Component s' f' g) s'), memo :: Map p (HTML Void (Coproduct f (ChildF p f') Unit)) }
```

The type used by component containers for their state where `s` is the
state local to the container, `p` is the type of slot used by the
container, and the remaining parameters are the type variables for the
child components.

#### `installedState`

``` purescript
installedState :: forall s s' f f' g p. (Ord p) => s -> InstalledState s s' f f' g p
```

Creates an initial `InstalledState` value for a component container based
on a state value for the container.

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
query :: forall s s' f f' g p i. (Functor g, Ord p) => p -> f' i -> Free (HalogenF s f (QueryF s s' f f' g p)) (Maybe i)
```

Queries a child component, for use within a parent component's `eval` or
`peek` function.

#### `query'`

``` purescript
query' :: forall s s' s'' f f' f'' g p p' i. (Functor g, Ord p') => ChildPath s s' f f' p p' -> p -> f i -> Free (HalogenF s'' f'' (QueryF s'' s' f'' f' g p')) (Maybe i)
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

#### `transformChild`

``` purescript
transformChild :: forall s s' f f' g p p' o q. (Functor g) => ChildPath s s' f f' p p' -> ComponentP s f g o q -> ComponentP s' f' g o q
```

#### `interpret`

``` purescript
interpret :: forall s f g g' o p. (Functor g') => Natural g g' -> ComponentP s f g o p -> ComponentP s f g' o p
```

Changes the component's `g` type. A use case for this would be to interpret
some `Free` monad as `Aff` so the component can be used with `runUI`.

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


