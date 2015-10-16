## Module Halogen.HTML

This module re-exports the core types for the `HTML` DSL, and values for
all supported HTML elements.

Consider using the `Halogen.HTML.Indexed` variety of this module for
better type safety.

#### `text`

``` purescript
text :: forall p i. String -> HTML p i
```

Constructs a text node `HTML` value.

#### `slot`

``` purescript
slot :: forall s f g p i. p -> (Unit -> { component :: Component s f g, initialState :: s }) -> HTML (SlotConstructor s f g p) i
```

Defines a slot for a child component. Takes a slot "address" value and a
thunked constructor.

#### `slot'`

``` purescript
slot' :: forall s s' f f' g p p' i. (Functor g) => ChildPath s s' f f' p p' -> p -> (Unit -> { component :: Component s f g, initialState :: s }) -> HTML (SlotConstructor s' f' g p') i
```

Defines a slot for a child component when a parent has multiple types of
child component. Takes the `ChildPath` for the child component's type, a
slot "address" value and a thunked constructor.


