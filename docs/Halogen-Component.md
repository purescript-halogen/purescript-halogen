# Module Documentation

## Module Halogen.Component


This module defines a type of composable _components_.

#### `Component`

``` purescript
newtype Component p m node req res
```

A component.

The type parameters are, in order:

- `p`, the type of _placeholders_
- `m`, the monad used to track effects required by external requests
- `node`, the type of node used to render content
- `req`, the type of external requests
- `res`, the type of external responses

Request and response types are public, but the component may also use an _internal_ type
of messages, as illustrated by the type of the `component` function.

The main interface to Halogen is the `runUI` function, which takes a component as an argument,
with certain constraints between the type arguments. This module leaves the type arguments
unrestricted, allowing components to be composed in various ways.

If you do not use a particular feature (e.g. placeholders, requests), you might like to leave 
the corresponding type parameter unconstrained in the declaration of your component. 

#### `component`

``` purescript
component :: forall p m node req res i. (Bifunctor node, Functor m) => SF1 req (node p (m res)) -> Component p m node req res
```

Create a component by providing a signal function.

The signal function should consume external requests and produce DOM nodes. The DOM
nodes in turn will create (monadic) external requests.

See the `Halogen.Signal` documentation.

#### `component'`

``` purescript
component' :: forall p m node req res i. SF1 (Either i req) (node p (m (Either i res))) -> Component p m node req res
```

A variant of `component` which creates a component with some internal, hidden input type.

#### `runComponent`

``` purescript
runComponent :: forall p m node req res r. (forall i. SF1 (Either i req) (node p (m (Either i res))) -> r) -> Component p m node req res -> r
```

Unpack a component.

The rank-2 type ensures that the hidden message type must be used abstractly.

#### `combine`

``` purescript
combine :: forall p m node req1 req2 res1 res2. (Bifunctor node, Functor m) => (forall a. node p a -> node p a -> node p a) -> Component p m node req1 res1 -> Component p m node req2 res2 -> Component p m node (Either req1 req2) (Either res1 res2)
```

Combine two components.

#### `profunctorComponent`

``` purescript
instance profunctorComponent :: (Bifunctor node, Functor m) => Profunctor (Component p m node)
```




