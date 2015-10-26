## Module Halogen.Query.SubscribeF

A part of the `HalogenF` algebra that allows subscription to event
listeners.

#### `EventSource`

``` purescript
type EventSource f g = Producer (f Unit) g Unit
```

A type alias for a coroutine producer used to represent a subscribable
source of events.

#### `eventSource`

``` purescript
eventSource :: forall eff a f. ((a -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit) -> (a -> Eff (avar :: AVAR | eff) (f Unit)) -> EventSource f (Aff (avar :: AVAR | eff))
```

Creates an `EventSource` for an event listener that accepts one argument.

- The first argument is the function that attaches the event listener.
- The second argument is a handler that produces a value in `f`.

For example:

``` purescript
let onCopied = eventSource (Editor.onCopy editor) \text -> do
      pure $ actionF (TextCopied text)
```
(Taken from the Ace component example)

#### `eventSource_`

``` purescript
eventSource_ :: forall eff f. (Eff (avar :: AVAR | eff) Unit -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) (f Unit) -> EventSource f (Aff (avar :: AVAR | eff))
```

Creates an `EventSource` for an event listener that accepts no arguments.

- The first argument is the function that attaches the event listener.
- The second argument is a handler that produces a value in `f`.

For example:

``` purescript
let onChange = eventSource_ (Session.onChange session) do
      text <- Editor.getValue editor
      pure $ actionF (ChangeText text)
```
(Taken from the Ace component example)

#### `SubscribeF`

``` purescript
data SubscribeF f g a
  = Subscribe (EventSource f g) a
```

The subscribe algebra.

##### Instances
``` purescript
instance functorSubscribeF :: Functor (SubscribeF f g)
```

#### `remapSubscribe`

``` purescript
remapSubscribe :: forall f g h. (Functor h) => Natural f g -> Natural (SubscribeF f h) (SubscribeF g h)
```

Changes the generating functor for an `EventSource`. Used internally by
Halogen.

#### `hoistSubscribe`

``` purescript
hoistSubscribe :: forall f g h. (Functor h) => Natural g h -> Natural (SubscribeF f g) (SubscribeF f h)
```

Changes the underlying monad for an `EventSource`. Used internally by
Halogen.

#### `transformSubscribe`

``` purescript
transformSubscribe :: forall f f' g g'. (Functor g, Functor g') => Natural f f' -> Natural g g' -> Natural (SubscribeF f g) (SubscribeF f' g')
```

#### `subscribeN`

``` purescript
subscribeN :: forall f g. (MonadRec g) => Consumer (f Unit) g Unit -> Natural (SubscribeF f g) g
```

A natural transformation for interpreting the subscribe algebra as its
underlying monad, via a coroutine consumer. Used internally by Halogen.


