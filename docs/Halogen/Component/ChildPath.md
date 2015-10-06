## Module Halogen.Component.ChildPath

#### `ChildPath`

``` purescript
data ChildPath s s' f f' p p'
  = ChildPath (Injector s s') (forall a. Injector (f a) (f' a)) (Injector p p')
```

Represents a path through `Either` and `Coproduct` types for the state,
query algebra, and slots of a component. Used when installing children of
different types within a single parent component.

#### `compose`

``` purescript
compose :: forall s t u f g h p q r. ChildPath t u g h q r -> ChildPath s t f g p q -> ChildPath s u f h p r
```

Composes two paths.

#### `(:>)`

``` purescript
(:>) :: forall s t u f g h p q r. ChildPath t u g h q r -> ChildPath s t f g p q -> ChildPath s u f h p r
```

_left-associative / precedence -1_

An infix alias for `compose`.

#### `cpL`

``` purescript
cpL :: forall s t f g p q. ChildPath s (Either s t) f (Coproduct f g) p (Either p q)
```

A `ChildPath` that represents taking the left-hand choice for each of a
component's `Either` and `Coproduct`s choices.

#### `cpR`

``` purescript
cpR :: forall s t f g p q. ChildPath s (Either t s) f (Coproduct g f) p (Either q p)
```

A `ChildPath` that represents taking the right-hand choice for each of a
component's `Either` and `Coproduct`s choices.

#### `cpI`

``` purescript
cpI :: forall s f p. ChildPath s s f f p p
```

An identity `ChildPath`.

#### `injState`

``` purescript
injState :: forall s s' f f' p p'. ChildPath s s' f f' p p' -> s -> s'
```

Uses a `ChildPath` definition to get a state value of type `s'` from a
value of type `s`. Used internally by Halogen.

#### `prjState`

``` purescript
prjState :: forall s s' f f' p p'. ChildPath s s' f f' p p' -> s' -> Maybe s
```

Uses a `ChildPath` to attempt to get a state value of type `s` from a value
of type `s'`. Used internally by Halogen.

#### `injQuery`

``` purescript
injQuery :: forall s s' f f' p p'. ChildPath s s' f f' p p' -> Natural f f'
```

Uses a `ChildPath` definition to get a query algebra value of type `f'`
from a value of type `f`. Used internally by Halogen.

#### `prjQuery`

``` purescript
prjQuery :: forall s s' f f' p p' a. ChildPath s s' f f' p p' -> f' a -> Maybe (f a)
```

Uses a `ChildPath` to attempt to get a query algebra value of type `f`
from a value of type `f'`. Used internally by Halogen.

#### `injSlot`

``` purescript
injSlot :: forall s s' f f' p p'. ChildPath s s' f f' p p' -> p -> p'
```

Uses a `ChildPath` definition to get a slot value of type `p'` from a
value of type `p`. Used internally by Halogen.

#### `prjSlot`

``` purescript
prjSlot :: forall s s' f f' p p'. ChildPath s s' f f' p p' -> p' -> Maybe p
```

Uses a `ChildPath` definition to get a slot value of type `p'` from a
value of type `p`. Used internally by Halogen.


