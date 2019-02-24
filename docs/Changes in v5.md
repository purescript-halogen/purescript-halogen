# Changes in v5

This is a crash-course guide to things that have changed between v4 and v5. Please open an issue or PR if you have suggestions about anything that is missing or improvements that can be made!

## Component construction

- There are no longer parent and child specific variations for the component constructors, HTML, and DSL types used when defining a component. All components now use `ComponentHTML` and `HalogenM`.

- There are no longer `lifecycle`-specific variations for component constructors either. That leaves us with a single constructor for components, `mkComponent`.

## Component eval

- Previously a component's query algebra defined everything a component could do. This is now split into "actions" and queries.

- Actions are internal to the component (they do not appear in the `Component` type signature), and are of kind `Type` rather than `Type -> Type`. Since actions can only be raised internally there is no need to parameterise them the way queries are, since there is no possibility of an action returning a value.

  Being "raised internally" means they arise from event listeners in the HTML and from `EventSource`s.

- Queries still as exist as a means of a parent component interacting with a child. They are not required to be used however - many components can be self contained and only need actions.

- Previously there was a concept of "action-style" queries that did not receive a return value (as opposed to "request-style" queries). These still exist, but are now termed "tell-style", and are constructed with `H.tell` rather than `H.action` to avoid overloading the "action" terminology.

- Query evaluation can now "fail" without resorting to throwing exceptions. Query eval is now `query a -> HalogenM ... (Maybe a)` rather than `query ~> HalogenM ...`. If the evaluator returns `Nothing` for a query, it will be flattened during the call to `H.query`, and result in `Nothing` - it's indistinguishable from the case where the component being queried does not exist at all.

- The `eval` for a component now deals with an input algebra `HalogenQ`:
  ``` purescript
  eval :: HalogenQ query action input ~> HalogenM state action slots output m
  ```
  `HalogenQ` has constructors for lifecycle, receiver, query, and actions:
  ``` purescript
  data HalogenQ query action input a
    = Initialize a
    | Finalize a
    | Receive input a
    | Action action a
    | Query (Coyoneda query a) (Unit → a)
  ```
  This can be pattern matched on and the cases handled as appropriate, but there is a `mkEval` helper that will probably be more convenient to use in most cases.

- `mkEval` accepts an `EvalSpec` record that looks a bit like the old lifecycle component constructor:
  ``` purescript
  type EvalSpec state query action slots input output m =
    { handleAction :: action -> HalogenM state action slots output m Unit
    , handleQuery :: forall a. query a -> HalogenM state action slots output m (Maybe a)
    , receive :: input -> Maybe action
    , initialize :: Maybe action
    , finalize :: Maybe action
    }
  ```
  There is an "empty" version of this called `defaultEval` that can be used and have the relevant fields overridden to reduce boilerplate:
  ``` purescript
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval (H.defaultEval { handleAction = ?handleAction })
    }
  ```
  If `defaultEval` is used with no overrides the component will do nothing for any action raised internally, and any queries made of it will fail.

- The `Halt` constructor was removed from `HalogenM`. If a component needs to explode in that way, it should be done by lifting something into the component's `m` instead.

  If `Halt` was being used for an infallible case in a higher order component `eval`, the same effect can be achieved now by returning `Nothing`.

  If this doesn't mean anything to you, don't worry about it! Halting wasn't explained anywhere previously and was used internally for the most part.

## Child component addressing

- TODO: rows, no childpath

- TODO: suggested `Slot` definition w/components

## Subscriptions, forking, and event sources

- `subscribe` now returns a `SubscriptionId` value that allows a subscription to later be cancelled with `unsubscribe`. Previously subscriptions could only be ended "from the inside", where the event source closes itself.

  It is still possible for a subscription to unsubscribe itself, the `subscribe'` function passes the `SubscriptionId` into a function that returns the `EventSource` so that the `EventSource` can raise an action with the relevant `SubscriptionId`.

- `fork` works similarly, in that it returns a `ForkId` rather than the canceler-function it used to. Forks are now cancelled by passing the `ForkId` to `H.kill`.

- The `EventSource` API was simplified. An `Emitter` type was added in an attempt to make the types more comprehensible, and the many variations of event source construction helpers were reduced to `affEventSource` and `effEventSource`, which should suffice for all common usages.


## Miscellaneous

- You can now `dispose` of an entire Halogen app via the `DriverIO` record returned from `runUI`. This will remove everything from the DOM and finalize the components. Attempting to `query` the `DriverIO` after this will return `Nothing`.

- TODO: memoized, thunks, lazy

- TODO: some examples better illustrate what they're doing

- TODO: `InputAcceptType` fix

- TODO: long var names
