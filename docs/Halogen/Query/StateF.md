## Module Halogen.Query.StateF

A part of the `HalogenF` algebra that replicates a `MonadState`-like
interface.

#### `StateF`

``` purescript
data StateF s a
  = Get (s -> a)
  | Modify (s -> s) a
```

The state algebra.

##### Instances
``` purescript
Functor (StateF s)
```

#### `mapState`

``` purescript
mapState :: forall s t a. (t -> s) -> ((s -> s) -> t -> t) -> StateF s a -> StateF t a
```

Map over the state value using a function to extract the new state value
from the old state, and a function for modifying the state.

#### `stateN`

``` purescript
stateN :: forall s m. (Monad m, MonadState s m) => Natural (StateF s) m
```

A natural transformation for interpreting the state algebra as some
`MonadState`-supporting monad. Used internally by Halogen.


