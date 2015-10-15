## Module Halogen.HTML

#### `text`

``` purescript
text :: forall p i. String -> HTML p i
```

#### `slot`

``` purescript
slot :: forall s f g p i. p -> Component s f g -> (Unit -> s) -> HTML (SlotConstructor s f g p) i
```

#### `slot'`

``` purescript
slot' :: forall s s' f f' g p p' i. (Functor g) => ChildPath s s' f f' p p' -> p -> Component s f g -> (Unit -> s) -> HTML (SlotConstructor s' f' g p') i
```


