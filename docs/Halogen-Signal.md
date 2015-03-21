# Module Documentation

## Module Halogen.Signal


This module defines signal functions (`SF`) and non-empty signal functions (`SF1`) and combinators
for working with them.

#### `SF`

``` purescript
newtype SF i o
```

A `SF` represents a state machine which responds to inputs of type `i`, producing outputs of type `o`.

#### `runSF`

``` purescript
runSF :: forall i o. SF i o -> i -> SF1 i o
```

Run a `SF` by providing an input

#### `SF1`

``` purescript
newtype SF1 i o
```

`SF1` represents non-empty signals, i.e. signals with an initial output value.

#### `runSF1`

``` purescript
runSF1 :: forall i o. SF1 i o -> { next :: SF i o, result :: o }
```

Run a `SF1` to obtain the initial value and remaining signal

#### `arr`

``` purescript
arr :: forall i o. (i -> o) -> SF i o
```

Create a `SF` from a function  

#### `input`

``` purescript
input :: forall i. SF i i
```

A `SF` which returns the latest input

#### `startingAt`

``` purescript
startingAt :: forall i o. SF i o -> o -> SF1 i o
```

Convert a `SF` to a `SF1` by providing an initial value

#### `head`

``` purescript
head :: forall i o. SF1 i o -> o
```

Get the current value of a `SF1`

#### `tail`

``` purescript
tail :: forall i o. SF1 i o -> SF i o
```

Convert a `SF1` to a `SF` by ignoring its initial value

#### `stateful`

``` purescript
stateful :: forall s i o. s -> (s -> i -> s) -> SF1 i s
```

Creates a stateful `SF1`

#### `stateful'`

``` purescript
stateful' :: forall s i o. s -> (s -> i -> Tuple o s) -> SF i o
```

Creates a stateful `SF` based on a function which returns an output value

#### `differencesWith`

``` purescript
differencesWith :: forall i d. (i -> i -> d) -> i -> SF i d
```

A `SF` which compares consecutive inputs using a helper function

#### `loop`

``` purescript
loop :: forall s i o. s -> SF (Tuple s i) (Tuple s o) -> SF i o
```

Create a `SF` which hides a piece of internal state of type `s`.

#### `functorSF`

``` purescript
instance functorSF :: Functor (SF i)
```


#### `functorSF1`

``` purescript
instance functorSF1 :: Functor (SF1 i)
```


#### `applySF`

``` purescript
instance applySF :: Apply (SF i)
```


#### `applySF1`

``` purescript
instance applySF1 :: Apply (SF1 i)
```


#### `applicativeSF`

``` purescript
instance applicativeSF :: Applicative (SF i)
```


#### `applicativeSF1`

``` purescript
instance applicativeSF1 :: Applicative (SF1 i)
```


#### `profunctorSF`

``` purescript
instance profunctorSF :: Profunctor SF
```


#### `profunctorSF1`

``` purescript
instance profunctorSF1 :: Profunctor SF1
```


#### `strongSF`

``` purescript
instance strongSF :: Strong SF
```


#### `choiceSF`

``` purescript
instance choiceSF :: Choice SF
```


#### `combineLatest`

``` purescript
combineLatest :: forall a b c d. SF1 a b -> SF1 c d -> SF1 (Either a c) (Tuple b d)
```

Combine two non-empty signals, outputting the latest value from both
signals at each step.

#### `semigroupoidSF`

``` purescript
instance semigroupoidSF :: Semigroupoid SF
```


#### `semigroupoidSF1`

``` purescript
instance semigroupoidSF1 :: Semigroupoid SF1
```


#### `categorySF`

``` purescript
instance categorySF :: Category SF
```




