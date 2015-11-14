## Module Halogen.Query

Functions and types used to describe the `HalogenF` algebra used in a
component's `eval` and `peek` functions.

#### `Action`

``` purescript
type Action f = Unit -> f Unit
```

Type synonym for an "action" - An action only causes effects and has no
result value.

In a query algebra, an action is any constructor that carries the algebra's
type variable as a value. For example:

``` purescript
data Query a
  = SomeAction a
  | SomeOtherAction String a
  | NotAnAction (Boolean -> a)
```

Both `SomeAction` and `SomeOtherAction` have `a` as a value so they are
considered actions, whereas `NotAnAction` has `a` as the result of a
function so is considered to be a "request" ([see below](#Request)).

#### `action`

``` purescript
action :: forall f. Action f -> f Unit
```

Takes a data constructor of query algebra `f` and creates an action.

For example:

```purescript
data Query a = Tick a

sendTick :: forall eff. Driver Query eff -> Aff (HalogenEffects eff) Unit
sendTick driver = driver (action Tick)
```

#### `Request`

``` purescript
type Request f a = forall i. (a -> i) -> f i
```

Type synonym for an "request" - a request can cause effects as well as
fetching some information from a component.

In a query algebra, an action is any constructor that carries the algebra's
type variable as the return value of a function. For example:

``` purescript
data Query a = SomeRequest (Boolean -> a)
```

#### `request`

``` purescript
request :: forall f a. Request f a -> f a
```

Takes a data constructor of query algebra `f` and creates a request.

For example:

```purescript
data Query a = GetTickCount (Int -> a)

getTickCount :: forall eff. Driver Query eff -> Aff (HalogenEffects eff) Int
getTickCount driver = driver (request GetTickCount)
```

#### `HalogenF`

``` purescript
data HalogenF s f g a
  = StateHF (StateF s a)
  | SubscribeHF (SubscribeF f g a)
  | QueryHF (g a)
  | HaltHF
```

The Halogen component algebra

##### Instances
``` purescript
instance functorHalogenF :: (Functor g) => Functor (HalogenF s f g)
instance injectStateHF :: Inject (StateF s) (HalogenF s f g)
instance injectSubscribeHF :: Inject (SubscribeF f g) (HalogenF s f g)
instance injectQueryHF :: Inject g (HalogenF s f g)
instance altHalogenF :: (Functor g) => Alt (HalogenF s f g)
instance plusHalogenF :: (Functor g) => Plus (HalogenF s f g)
```

#### `get`

``` purescript
get :: forall s f g. Free (HalogenF s f g) s
```

Provides a way of accessing the current component's state within an `Eval`
or `Peek` function. This is much like `get` for the `State` monad, but
instead of operating in some `StateT`, uses the `HalogenF` algebra. For
example:

``` purescript
data Query a = GetState (State -> a)

eval :: forall g. Eval Query State Query g
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
data Query a = GetX (Number -> a)
type State = { x :: Number, y :: Number }

eval :: forall g. Eval Query State Query g
eval (GetX k) = do
  x <- gets _.x
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
data Query a = Increment a
type State = Int

eval :: forall g. Eval Query State Query g
eval (Increment next) = do
  modify (+ 1)
  pure next
```

#### `subscribe`

``` purescript
subscribe :: forall s f g. EventSource f g -> Free (HalogenF s f g) Unit
```

Provides a way of having a component subscribe to an `EventSource` from
within an `Eval` function.

#### `subscribe'`

``` purescript
subscribe' :: forall s s' f f' g. EventSource f g -> Free (HalogenF s f (Free (HalogenF s' f' g))) Unit
```

Provides a way of having a parent component subscribe to an `EventSource`
from within an `Eval` or `Peek` function.

#### `liftH`

``` purescript
liftH :: forall a s f g. g a -> Free (HalogenF s f g) a
```

A convenience function for lifting a `g` value directly into
`Free HalogenF` without the need to use `liftF $ right $ right $ ...`.

#### `liftAff'`

``` purescript
liftAff' :: forall eff a s f g. (MonadAff eff g, Functor g) => Aff eff a -> Free (HalogenF s f g) a
```

A convenience function for lifting an `Aff` action directly into a
`Free HalogenF` when there is a `MonadAff` instance for the current `g`,
without the need to use `liftH $ liftAff $ ...`.

#### `liftEff'`

``` purescript
liftEff' :: forall eff a s f g. (MonadEff eff g, Functor g) => Eff eff a -> Free (HalogenF s f g) a
```

A convenience function for lifting an `Eff` action directly into a
`Free HalogenF` when there is a `MonadEff` instance for the current `g`,
without the need to use `liftH $ liftEff $ ...`.

#### `transformHF`

``` purescript
transformHF :: forall s s' f f' g g'. (Functor g, Functor g') => Natural (StateF s) (StateF s') -> Natural f f' -> Natural g g' -> Natural (HalogenF s f g) (HalogenF s' f' g')
```

Change all the parameters of `HalogenF`.

#### `hoistHalogenF`

``` purescript
hoistHalogenF :: forall s f g h. (Functor h) => Natural g h -> Natural (HalogenF s f g) (HalogenF s f h)
```

Changes the `g` for a `HalogenF`. Used internally by Halogen.


