## Module Halogen.Query

#### `action`

``` purescript
action :: forall f. (Unit -> f Unit) -> f Unit
```

Takes a data constructor of query algebra `f` and creates an "action". An
"action" only causes effects and has no result value.

For example:

```purescript
data Input a = Tick a

sendTick :: forall eff. Driver Input eff -> Aff (HalogenEffects eff) Unit
sendTick driver = driver (action Tick)
```

#### `request`

``` purescript
request :: forall f a. (forall i. (a -> i) -> f i) -> f a
```

Takes a data constructor of query algebra `f` and creates a "request". A
"request" can cause effects as well as fetching some information from a
component.

For example:

```purescript
data Input a = GetTickCount (Int -> a)

getTickCount :: forall eff. Driver Input eff -> Aff (HalogenEffects eff) Int
getTickCount driver = driver (request GetTickCount)
```

#### `HalogenF`

``` purescript
type HalogenF s f g = Coproduct (StateF s) (Coproduct (SubscribeF f g) g)
```

A type alias for the full Halogen component algebra.

#### `get`

``` purescript
get :: forall s f g. Free (HalogenF s f g) s
```

Provides a way of accessing the current component's state within an `Eval`
or `Peek` function. This is much like `get` for the `State` monad, but
instead of operating in some `StateT`, uses the `HalogenF` algebra. For
example:

``` purescript
data Input a = GetState (State -> a)

eval :: forall g. (Functor g) => Eval Input (Free Input) State g
eval (GetState k) = do
  currentState <- get
  pure (k currentState)
```

#### `gets`

``` purescript
gets :: forall s f g a. (s -> a) -> Free (HalogenF s f g) a
```

A version of [`get`](#get) that maps over the retrieved state before
returning the result. Useful in cases where only a portion of the state is
desired. For example:

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
modify :: forall s f g. (s -> s) -> Free (HalogenF s f g) Unit
```

Provides a way of modifying the current component's state within an `Eval`
or `Peek` function. This is much like `modify` for the `State` monad, but
instead of operating in some `StateT`, uses the `HalogenF` algebra. For
example:

``` purescript
data Input a = Increment a
type State = Int

eval :: Eval Input (Free Input) State g
eval (Increment next) = do
  modify (+ 1)
  pure next
```

#### `subscribe`

``` purescript
subscribe :: forall s f g. EventSource f g -> Free (HalogenF s f g) Unit
```

Provides a way of having a component subscribe to an `EventSource` from
within an `Eval` or `Peek` function.


