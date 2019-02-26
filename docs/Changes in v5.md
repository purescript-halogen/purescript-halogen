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

- If you want to share some of the behaviour between actions and queries without duplicating constructors and/or implementation, add a constructor like `EvalQuery (Query Unit)` to the component's action type, and then pass the `Query Unit` value to `handleQuery` for that case in `handleAction`.

  It's also possible to add an `EvalAction Action a` to the query type for the component instead, but doing it the other way around is recommended. The action type can be used to hide internal interactions that shouldn't be called externally, but the query is always fully public.

- The `Halt` constructor was removed from `HalogenM`. If a component needs to explode in that way, it should be done by lifting something into the component's `m` instead.

  If `Halt` was being used for an infallible case in a higher order component `eval`, the same effect can be achieved now by returning `Nothing`.

  If this doesn't mean anything to you, don't worry about it! Halting wasn't explained anywhere previously and was used internally for the most part.

## Child component addressing

- The component `HTML` and `DSL` (`HalogenM`) types now have a single type variable, `slots`, that determines all the information necessary for child components.

  Previously this was split across two arguments, one for the query type(s) for child components, and another for the slot value(s). When a component had multiple types of child component, these types got unpleasant to deal with, as they needed nested coproduct or either types to accommodate everything.

  The new `slots` is a row type, using the labels as identifiers for the different child component types, and associating a `H.Slot` value with each label, specifying the query and output message type for the child component, and the type to index the component by.

  For example, an old-style setup might look something like this:

  ``` purescript
  type ChildQuery = Coproduct3 CA.Query CB.Query CC.Query

  type ChildSlot = Either3 Unit Unit Unit

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render state =
    HH.div_
      [ HH.slot' CP.cp1 unit CA.component unit absurd
      , HH.slot' CP.cp2 unit CB.component unit absurd
      , HH.slot' CP.cp3 unit CC.component unit absurd
      ]
  ```

  Now it'd be expressed as:

  ``` purescript
  type ChildSlots =
    ( a :: H.Slot CA.Query Void Unit
    , b :: H.Slot CB.Query Void Unit
    , c :: H.Slot CC.Query Void Unit
    )

  _a = SProxy :: SProxy "a"
  _b = SProxy :: SProxy "b"
  _c = SProxy :: SProxy "c"

  render :: forall m. State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div_
      [ HH.slot _a unit CA.component unit absurd
      , HH.slot _b unit CB.component unit absurd
      , HH.slot _c unit CC.component unit absurd
      ]
  ```

  On the surface of it this might look pretty similar to what was there before, but in practice it is _much_ nicer to deal with! Especially if you're one of the people out there who needed more than 10 types of child component, since we only provided helper types and premade `ChildPath` values up to that.

- With the change to the single `slots` type variable, there is now only one `HH.slot` function for rendering and `HH.query` for querying. Previously there was a non-primed version inteded for components that only had one type of child component, which allowed the `ChildPath` argument to be skipped.

  Now an `SProxy` must always be provided to `HH.slot` or `HH.query`, even if there is only one component to choose from, since it would still need to be defined as a `label :: slot` pair in the `slots` row.

- A pattern that has emerged in our usage of the new setup, is to define a `Slot` type synonym along with each component, filling in the query and message values for `H.Slot` but leaving the last argument unspecified.

  So from the above, the `CA`, `CB`, `CC` modules would each have a definition like:
  ``` purescript
  type Slot = H.Slot Query Void
  ```
  Then when used in a parent component the row type can be simplified:
  ``` purescript
  type ChildSlots =
    ( a :: CA.Slot Unit
    , b :: CB.Slot Unit
    , c :: CC.Slot Unit
    )
  ```
  Leaving the last argument (the slot index value) unspecified allows the parent to decide on a suitable value to use there (`Unit` for if there's only going to be one instance of that type of component, some other type like `Int` or `String` if there are going to be multiple instances).

## Subscriptions, forking, and event sources

- `subscribe` now returns a `SubscriptionId` value that allows a subscription to later be cancelled with `unsubscribe`. Previously subscriptions could only be ended "from the inside", where the event source closes itself.

  It is still possible for a subscription to unsubscribe itself, the `subscribe'` function passes the `SubscriptionId` into a function that returns the `EventSource` so that the `EventSource` can raise an action with the relevant `SubscriptionId`.

- `fork` works similarly, in that it returns a `ForkId` rather than the canceler-function it used to. Forks are now cancelled by passing the `ForkId` to `H.kill`.

  Aditionally, `fork`s are killed when a component is finalized now, unless the `fork` takes place during finalization.

- The `EventSource` API was simplified. An `Emitter` type was added in an attempt to make the types more comprehensible, and the many variations of event source construction helpers were reduced to `affEventSource` and `effEventSource`, which should suffice for all common usages.


## Miscellaneous

- You can now `dispose` of an entire Halogen app via the `DriverIO` record returned from `runUI`. This will remove everything from the DOM and finalize the components. Attempting to `query` the `DriverIO` after this will return `Nothing`.

- The examples have been changed to try and best illustrate the feature they relate to, and just generally tidied up a bit. Some specifics:

  - The `interpret` example now works on a component that is using a `ReaderT` over `Aff` rather than a `Free` monad. `ReaderT` + `Aff` is a very common real world setup for an app's effect monad.

  - The `higher-order-components` example shows a expandable/collapsible container box kind of thing that allows interactions with the inner component when it is expanded.

  - The `todo` example has gone, as it was intended to show a fairly-but-not-entirely trivial example, but had weird conventions that nobody uses. @thomashoneyman's [Real World Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld) is a much better and more comprehensive example of how an app might be structured (although it's out of date now, of course!).

- The `accept` property (for file inputs) didn't have quite the right type before, it accepted a `MediaType`, but really should have allowed a collection of media types and file extensions. The type has been changed to a new `InputAcceptType` monoid to fix this.

- The type variables have been renamed to full words in the component / query / etc. type signatures. Maybe this will help, maybe not - feedback is welcome and appreciated!
