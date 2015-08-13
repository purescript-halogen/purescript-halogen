## Module Halogen.Query.StateF

A part of the `HalogenF` algebra that replicates a `MonadState`-like
interface, used to represent state changes in a component in the result of
a component’s `eval` function.

#### `StateF`

``` purescript
data StateF s a
  = Get (s -> a)
  | Modify (s -> s) a
```

The state algebra.

##### Instances
``` purescript
instance functorStateF :: Functor (StateF s)
```

#### `get`

``` purescript
get :: forall f s. (Inject (StateF s) f) => Free f s
```

Injects a `Get` action into a `Free` monad that is making use of the state
algebra.

This allows `get` to be used in a similar way to the version for the
`State` monad when operating in the `eval` function for a component.
For example:

``` purescript
data Input a = GetState (State -> a)

eval :: forall g. (Functor g) => Eval Input (Free Input) State g
eval (GetState k) = do
  currentState <- get
  pure (k currentState)
```

#### `gets`

``` purescript
gets :: forall f s a. (Inject (StateF s) f) => (s -> a) -> Free f a
```

A version of `get` that maps over the retrieved state before returning the
result. Useful in cases where only a portion of the state is desired, for
example:

``` purescript
data Input a = GetX (Number -> a)
newtype State = State { x :: Number, y :: Number }

eval :: forall g. (Functor g) => Eval Input (Free Input) State g
eval (GetX k) = do
  x <- gets \(State st) -> st.x
  pure (k x)
```

#### `modify`

``` purescript
modify :: forall f s. (Inject (StateF s) f) => (s -> s) -> Free f Unit
```

Injects a `Modify` action into a `Free` monad that is making use of the
state algebra.

This allows `modify` to be used in a similar way to the version for the
`State` monad when operating in the `eval` function for a component.
For example:

``` purescript
data Input a = Increment a
type State = Int

eval :: Eval Input (Free Input) State g
eval (Increment next) = do
  modify (+ 1)
  pure next
```

#### `stateN`

``` purescript
stateN :: forall s m. (Monad m, MonadState s m) => Natural (StateF s) m
```

A natural transformation for interpreting the state algebra as some
`MonadState`-supporting monad. Used internally by Halogen in `runUI`.


