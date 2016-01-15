## Module Halogen.Query.EventSource

#### `EventSource`

``` purescript
newtype EventSource f g
  = EventSource (StallingProducer (f Unit) g Unit)
```

#### `runEventSource`

``` purescript
runEventSource :: forall f g. EventSource f g -> StallingProducer (f Unit) g Unit
```

#### `eventSource`

``` purescript
eventSource :: forall eff a f g. (Monad g, MonadAff (avar :: AVAR | eff) g) => ((a -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit) -> (a -> Eff (avar :: AVAR | eff) (f Unit)) -> EventSource f g
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
eventSource_ :: forall eff f g. (Monad g, MonadAff (avar :: AVAR | eff) g) => (Eff (avar :: AVAR | eff) Unit -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) (f Unit) -> EventSource f g
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

#### `catEventSource`

``` purescript
catEventSource :: forall f g. (MonadRec g) => EventSource (Coproduct (Const Unit) f) g -> EventSource f g
```

Take an `EventSource` with events in `1 + f` to one with events in `f`.
This is useful for simultaneously filtering and handling events.

#### `ParentEventSource`

``` purescript
data ParentEventSource :: (* -> *) -> (* -> *) -> *
```

#### `toParentEventSource`

``` purescript
toParentEventSource :: forall f g h. EventSource f h -> ParentEventSource f (Free (g h))
```

#### `fromParentEventSource`

``` purescript
fromParentEventSource :: forall f g h. ParentEventSource f (Free (g h)) -> EventSource f h
```


