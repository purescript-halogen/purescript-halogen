# Changes in v5

This is a crash-course guide to things that have changed from Halogen 4 to Halogen 5. Please open an issue or a PR if you notice missing information or ways this transition guide could be improved!

Halogen 5 introduces many improvements to Halogen's performance and usability. If you are migrating an application from Halogen 4 we recommend reading through the full transition guide. However, you can also hop directly to a relevant section using the table of contents below.

1. [Component Constructors, HTML, and DSL Types](#component-constructors-html-and-dsl-types)
2. [Queries and Actions](#queries-and-actions)
3. [Component Evaluation](#component-evaluation)
4. [Child Component Addressing](#child-component-addressing)
5. [Subscriptions, Forking, and Event Sources](#subscriptions-forking-and-event-sources)
6. [Performance Optimization with Lazy and Memoized](#performance-optimization-with-lazy-and-memoized)
7. [Other Changes](#other-changes)

## Component Constructors, HTML, and DSL Types

Halogen 4 distinguished among parent- and child-specific for the HTML and DSL types used when defining a component, and between parent-, child-, and lifecycle-specific functions for constructing components.

Halogen 5 uses only one component constructor function, `mkComponent`, one type for HTML, `ComponentHTML`, and one type for component evaluation, `HalogenM`.

For example, a parent component would previously be defined with the `parentComponent` constructor and use the `ParentHTML` and `ParentDSL` type synonyms:

```purs
parentComponent :: H.Component HH.HTML Query Input Message m
parentComponent =
  H.parentComponent
    ...
  where
  render :: State  -> H.ParentHTML Query ChildQuery Slots m

  eval
    :: Query
    ~> H.ParentDSL State Query ChildQuery Slots Message m
```

Whereas a child component would be defined with the `component` constructor and use the `ComponentHTML` and `ComponentDSL` type synonyms:

```purs
childComponent :: H.Component HH.HTML Query Input Message m
childComponent =
  H.component
    ...
  where
  render :: State -> H.ComponentHTML Query

  eval :: Query ~> H.ComponentDSL State Query Message m
```

A component which used lifecycles (an initializer and/or finalizer) would be constructed with yet another pair of constructor functions:

```purs
parentComponentWithLifecycles = H.lifecycleParentComponent ...
childComponentWithLifecycles = H.lifecycleComponent ...
```

In Halogen 5, the only component constructor is `mkComponent`, the only type for HTML is `ComponentHTML`, and the only type for component evaluation is `HalogenM`.

Due to changes in queries and evaluation in Halogen 5, these types are not the same as they were in Halogen 4. We'll explore those changes in the next section.

## Queries and Actions

In Halogen 4, a component's query algebra defines everything the component can do. In Halogen 5, queries are only for parent-child communication, and a simpler action type is used within the component.

Previously, queries were the only type for defining computations the component can run. Queries were paired with the `eval` function, which defines the computation that should run when a query happens. There were two ways to write a query: "action-style" and "request-style":

```purs
data Query a
  = HandleClick a
  | RespondWithInt (Int -> a)
```

Action-style queries like `HandleClick` don't return anything when they are run by the `eval` function, whereas request-style queries like `RespondWithInt` do return a result. Correspondingly, action-style queries were typically used to handle events arising from HTML or event sources, and request-style queries were used for parent-child component communication.

In Halogen 5 this distinction has been made explicit. Components now use two separate types to represent computations: a query type for parent-child communication and an action type for internal events (like those arising from HTML or event sources).

The above query type from Halogen 4 would become, in Halogen 5, these two definitions:

```purs
-- Actions don't need to be parameterised because they can't
-- return a value. Actions are used instead of queries in
-- ComponentHTML and to handle event sources.
data Action
  = HandleClick

-- Queries are the same as they were in Halogen 4, but are
-- used specifically for parent-child communication instead of
-- being used to represent all computations in a component.
data Query a
  = RespondWithInt (Int -> a)
```

Actions don't show up in the type of the component because they cannot be accessed outside of the component:

```purs
component :: forall m. H.Component Query Input Output m
```

### Changes to Query Evaluation

Queries are still used as the public interface for a component, which means they are useful for parent-child communication. They aren't required, however: many components are self-contained and only need actions.

There have been a few other tweaks to queries in Halogen 5 worth knowing about.

You can still write "action-style" queries, but to avoid terminology overloading, they're now termed "tell-style" queries and are constructed using `H.tell` instead of `H.action`.

```purs
data MyQuery a
  = DoSomething a

-- Halogen 4
result <- H.query ... $ H.action DoSomething

-- Halogen 5
result <- H.query ... $ H.tell DoSomething
```

In addition, query evaluation in Halogen 5 can now "fail" without resorting to throwing exceptions. Query evaluation in Halogen 5 is now of the type:

```purs
query a -> HalogenM ... (Maybe a)
```

instead of the Halogen 4 type:

```purs
query ~> HalogenM ...
```

If evaluation returns `Nothing` for a query, then it will be flattened during the call to `H.query` and become indistinguishible from the case in which the component being queried doesn't exist.

### Introducing Actions

Actions are now used to represent computations internal to a component. They are of the kind `Type` instead of `Type -> Type` because, unlike queries, they can't return anything.

```purs
data Action
  = Increment
  | Decrement
```

Internally, actions are evaluated similarly to how queries are evaluated, with a function of the type:

```purs
action -> HalogenM ... Unit
```

This action type is now used in place of the query type in your render function:

```purs
-- Halogen 4
render :: State -> H.ParentHTML Query ChildQuery Slots m
render :: State -> H.ComponentHTML Query

-- Halogen 5
render :: State -> H.ComponentHTML Action Slots m
```

We're no longer using `Query` in the the Halogen 5 version. (We're not using `ChildQuery` either, but that's unrelated -- that's due to changes in how slots work in Halogen 5, which we'll address in a moment.)

One last thing about actions: since they are not of kind `Type -> Type`, helper functions like `input` and `input_` are no longer necessary when handling events in HTML, and so they have been removed in Halogen 5

```purs
-- Halogen 4
module Halogen.HTML.Events where

type Action f = Unit -> f Unit

input  :: forall f a. (a -> Action f) -> a -> Maybe (f Unit)
input_ :: forall f a. Action f -> a -> Maybe (f Unit)
```

In Halogen 4 these functions were used to transform queries in the render function:

```purs
-- Halogen 4
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a
  = Toggle a
  | Hover MouseEvent a

render :: State -> H.ComponentHTML Query
render =
  HH.button
    [ HE.onClick (HE.input_ Toggle)
    , HE.onMouseOver (HE.input Hover)
    ]
    [ HH.text "Click me" ]

```

This is how you'd write the same code in Halogen 5:

```purs
-- Halogen 5

data Action
  = Toggle
  | Hover MouseEvent

render :: forall m. State -> H.ComponentHTML Action Slots m
render =
  HH.button
    [ HE.onClick \_ -> Just Toggle
    , HE.onMouseOver (Just <<< Hover)
    ]
    [ HH.text "Click me" ]
```

### Mixing Queries and Actions

Now that actions and queries have been split apart you may want to share some of the behavior between actions and queries without duplicating the constructors and/or implementation. You can do that by adding a constructor to your action type which allows you to use your action-style queries:

```purs
data Query a
  = UpdateState a

data Action
  = HandleClick
  | EvalQuery (Query Unit)
```

Then, you can evaluate the "action-style" query when it arises as an action by unwrapping it and passing it your query evaluation function.

While it's also possible to add an `EvalAction Action a` constructor to your query type, this isn't recommended. The action type can be used to hide internal interactions that shouldn't be called externally, but the query type is always fully public.

## Component Evaluation

Component evaluation has changed now that there is only one constructor, `mkComponent`, no differentiation between child, parent, and lifecycle components, and an explicit separation between actions and queries.

In Halogen 4, the `component` constructor had separate fields for the `eval` function (handling queries) and the `receiver` function (handling component input), and the `lifecycleComponent` had additional fields for `initializer` and `finalizer` to handle lifecycle events.

In Halogen 5, the `mkComponent` constructor has just a single evaluation function, `eval`, which handles all the various kinds of events a component can encounter, including lifecycles, component input, queries, and actions.

```purs
eval
  :: HalogenQ query action input
  ~> HalogenM state action slots output m
```

In a moment we'll examine the `eval` function in-depth, but in most cases you'll construct it with the `mkEval` helper function paired with `defaultEval`, which provides default values for handling each of these cases. Here are a few different eval functions which handle various cases:

```purs
-- This eval function does nothing
H.mkComponent
  { initialState: ...
  , render: ...
  , eval: H.mkEval H.defaultEval
  }

-- This one handles only actions
eval = H.mkEval $ H.defaultEval
  { handleAction = \action - > ...
  }

-- This one handles actions, queries, and initialization:
data Action = Initialize

eval = H.mkEval $ H.defaultEval
  { handleAction = \action -> ...
  , handleQuery = \query -> ...
  , initialize = Just Initialize
  }
```

As you can tell, the `eval` function is no longer just for handling queries. Instead, it handles all the cases expressed by `HalogenQ`, a type that captures the various sorts of input that can be evaluated in a component:

```purs
data HalogenQ query action input a
  = Initialize a
  | Finalize a
  | Receive input a
  | Action action a
  | Query (Coyoneda query a) (Unit -> a)
```

You can write an `eval` function manually by pattern-matching on each of these constructors, but in most cases you should use the new `mkEval` helper function. This function accepts a record that looks similar to the old `lifecycleComponent` constructor:

```purs
type EvalSpec state query action slots input output m =
   { handleAction
       :: action
       -> HalogenM state action slots output m Unit
   , handleQuery
       :: forall a
        . query a
       -> HalogenM state action slots output m (Maybe a)
   , receive :: input -> Maybe action
   , initialize :: Maybe action
   , finalize :: Maybe action
   }
```

The `defaultEval` function provides default values for each of these handlers, which do nothing, and which you can override using ordinary PureScript record syntax:

```purs
-- This eval function uses the defaults, but overrides the
-- `handleAction` and `handleQuery` functions.
eval = H.mkEval $ H.defaultEval
  { handleAction = case _ of ...
  , handleQuery = case _ of ...
  }
```

## Child Component Addressing

Halogen 4 used two types to determine information necessary to render and query child components: the child component query type and a slot value used to identify a particular child component.

These types were unpleasant to work with when a component had multiple types of child component because they required nested `Coproduct` and `Either` types to accomodate everything, and you had to remember the order you listed your child component types in when using the `slot` or `query` functions.

```purs
-- Halogen 4

type ChildQuery =
  Coproduct3
    ComponentA.Query
    ComponentB.Query
    ComponentC.Query

type ChildSlot = Either3 Unit Int Unit

render :: forall m. State -> H.ParentHTML Query ChildQuery ChildSlot m
render state =
  HH.div_
    [ HH.slot' CP.cp1 ComponentA.component unit absurd
    , HH.slot CP.cp2 1 ComponentB.component unit absurd
    , HH.slot' CP.cp3 ComponentC.component unit absurd
    ]
```

In Halogen 5, all of this has been consolidated to a single row type where labels identify different child component types and the label's associated `H.Slot` value specifies the query, output, and slot type for the child component.

We can replace the `ChildQuery` and `ChildSlot` types with a single row type:

```purs
-- Halogen 5
type Slots =
  ( a :: H.Slot ComponentA.Query Void Unit
  , b :: H.Slot ComponentB.Query Void Int
  , c :: H.Slot ComponentC.Query Void Unit
  )
```

Instead of using `ChildPath` types (`cp1`, `cp2`, `cp3`, etc.) to identify components and slots, we now use symbol proxies for the labels in the row:

```purs
_a = SProxy :: SProxy "a"
_b = SProxy :: SProxy "b"
_c = SProxy :: SProxy "c"

render :: forall m. State -> H.ComponentHTML Action Slots m
render state =
  HH.div_
    [ HH.slot _a unit ComponentA.component unit absurd
    , HH.slot _b 1 ComponentB.component unit absurd
    , HH.slot _c unit ComponentC.component unit absurd
    ]
```

This may look similar on the surface to the prior non-row child query and child slot types, but in practice it is _much_ nicer to deal with -- especially if you were one of the people out there who needed more than 10 types of child component, as we only provided helper types and premade `ChildPath` values up to that.

In Halogen 4 the `slot`, `query`, and `queryAll` had primed variants, `slot'`, `query'`, and `queryAll'`, where the non-primed variants let you skip the `ChildPath` argument for components with only one type of child component.

In Halogen 5 there are only the un-primed variants. You must always provide an `SProxy` to the `slot`, `query`, and `queryAll` functions to identify the child component you are targeting.

The new row-based approach allows you greater flexibility to define helpers that work on slot types. For example, a common pattern in Halogen 5 applications is to define a `Slot` type synonym for a component in the same module in which the component is defined. This type synonym can specify the query and message types but leave the slot value unspecified, for a parent component to choose.

For example, if each of the `ComponentA`, `ComponentB`, and `ComponentC` modules in the example above had been defined with a type synonym for their slot type already:

```purs
module ComponentA where

type Slot = H.Slot Query Void

data Query = ...

component :: forall i o m. H.Component Query i Void m
```

Then parent components don't need to worry about specifying the query or message types for the child component:

```purs
type Slots =
  ( a :: ComponentA.Slot Unit
  , b :: ComponentB.Slot Int
  , c :: ComponentC.Slot Unit
  )
```

## Subscriptions, Forking, and Event Sources

Halogen 5 introduces a number of ergonomic improvements to subscriptions, forking, and event sources, including a new `EventSource` API.

### Subscriptions

The `subscribe` function in Halogen 5 now returns a `SubscriptionId` value that allows a subscription to be cancelled later with `unsubscribe`. Subscriptions could previously only be ended in response to an event -- the event source would close itself.

It's still possible for a subscription to unsubscribe itself. The `subscribe'` function passes the `SubscriptionId` into a function which returns the `EventSource`. That way the `EventSource` can raise an action with the relevant `SubscriptionId`.

### Event Sources

Halogen 5 simplifies the `EventSource` API by introducing a new `Emitter` type and reducing the many, many variations of event source construction helpers to just `affEventSource`, `effectEventSource`, and `eventListenerEventSource`. Event sources now use queries instead of actions, and no longer require event handlers to return a subscription status.

Event sources have simpler types in Halogen 5:

```purs
-- Halogen 4
newtype EventSource f m =
  EventSource (m
    { producer :: CR.Producer (f SubscribeStatus) m Unit
    , done :: m Unit
    })

-- Halogen 5
newtype EventSource m a =
  EventSource (m
    { producer :: CR.Producer a m Unit
    , finalizer :: Finalizer m
    })
```

But it's not common to manually create an event source. Instead, you should use the new `affEventSource` and `effectEventSource` helper functions:

```purs
affEventSource
  :: forall m a
   . MonadAff m
  => (Emitter Aff a -> Aff (Finalizer Aff))
  -> EventSource m a

effectEventSource
  :: forall m a
   . MonadAff m
  => (Emitter Effect a -> Effect (Finalizer Effect))
  -> EventSource m a
```

These functions let you set up a new event source from a setup function. This setup function operates in `Aff` or `Effect` and allows you to emit actions to the current component (or close the event source) using the `Emitter`. The setup function returns a `Finalizer` to run when the event source is unsubscribed or the emitter is closed.

The `emit` function allows you to emit an action using the emitter provided by the `affEventSource` and `effectEventSource` functions. The `close` function lets you close the emitter and shut down the event source.

For example, this example creates an event source which will emit the `Notify` action after one second and then close the event source:

```purs
data Action = Notify String

myEventSource :: EventSource Aff Action
myEventSource = EventSource.affEventSource \emitter -> do
  Aff.delay (Milliseconds 1000.0)
  EventSource.emit emitter (Notify "hello")
  EventSource.close emitter
  pure mempty
```

There is also an `eventListenerEventSource` function which you can use to set up an event source that listens to events in the DOM.

```purs
eventListenerEventSource
  :: forall m a
   . MonadAff m
  => EventType
  -> EventTarget
  -> (Event -> Maybe a)
  -> EventSource m a
```

For example, we can subscribe to changes in the browser window width:

```purs
data Action = Handler Window

handleAction = case _ of
  Initialize ->
    void $ H.subscribe do
      ES.eventListenerEventSource
        (EventType "resize")
        (Window.toEventTarget window)
        (Event.target >>> map (fromEventTarget >>> Handler))

  Handler window ->
    width <- liftEffect (innerWidth window)
    -- ...do something with the window width
```

When using event sources in components, you no longer need to respond to events with a `SubscribeStatus`:

```purs
-- Halogen 4
eval = case _ of
  HandleChange reply -> do
    -- ... your code
    pure (reply H.Listening)

-- Halogen 5
handleAction = case _ of
  HandleChange ->
    -- ... your code
```

### Forks

In Halogen 4 the `H.fork` function returned a canceller function.

In Halogen 5 it returns a `ForkId`, which you can pass to the `H.kill` function to cancel the fork. This mirrors the `H.subscribe` function. Forks are now killed when a component is finalized, unless the fork occurred during finalization.

## Performance Optimization with Lazy and Memoized

Halogen 5 introduces the ability to skip rendering for arbitrary HTML trees, not just at component boundaries as was the case in Halogen 4.

The new `memoized` function lets you skip rendering a tree of HTML given an equality predicate. If an argument is deemed equivalent to the value in the previous render then rendering and diffing will be skipped.

```purs
memoized
  :: forall a action slots m
   . (a -> a -> Boolean)
  -> (a -> ComponentHTML action slots m)
  -> a
  -> ComponentHTML action slots m
```

For example, you can skip rendering for equal state values by wrapping your component's render function:

```purs
myComponent = component
  { ...
  , render: memoized eq render
  , ...
  }
```

You can also skip rendering for referentially-equal arguments using the `lazy`, `lazy2`, and `lazy3` functions. These work like `memoized`, but instead of taking an equality predicate they use referential equality.

Here's an example of skipping rendering a large list of items when the state it depends on is unchanged between renders:

```purs
-- Before
render state =
  HH.div_ [ generateItems state.totalItems ]

-- After
render state =
  HH.div_ [ HH.lazy generateItems state.totalItems ]
```

These functions are a convenient way to wring extra performance out of your render code.

## Other Changes

Halogen 5 has also seen a number of other miscellaneous changes. These are quality of life improvements that don't affect many common workflows but which are worth noting.

### `Halt` and HalogenM

The `Halt` constructor was removed from `HalogenM`. If a component needs to explode in that way, it should be done by lifting something into the component's `m` instead.

If `Halt` was being used for an infallible case in a higher order component `eval`, the same effect can be achieved now by returning `Nothing`.

If this doesn't mean anything to you, don't worry about it! Halting wasn't explained anywhere previously and was used internally for the most part.

### App Disposal

You can now `dispose` of an entire Halogen app via the `DriverIO` record returned from `runUI`. This will remove everything from the DOM and finalize the components. Attempting to `query` the `DriverIO` after this will return `Nothing`.

### Updated Examples

The examples have been changed to try and best illustrate the feature they relate to, and just generally tidied up a bit. Some specifics:

- The `interpret` example now works on a component that is using a `ReaderT` over `Aff` rather than a `Free` monad. `ReaderT` + `Aff` is a very common real world setup for an app's effect monad.
- The `higher-order-components` example shows a expandable/collapsible container box kind of thing that allows interactions with the inner component when it is expanded.
- The `todo` example has gone, as it was intended to show a fairly-but-not-entirely trivial example, but had weird conventions that nobody uses. [@thomashoneyman](https://github.com/thomashoneyman)'s [Real World Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld) is a much better and more comprehensive example of how an app might be structured and is up-to-date for Halogen 5.

### File Inputs

The `accept` property (for file inputs) didn't have quite the right type before, it accepted a `MediaType`, but really should have allowed a collection of media types and file extensions. The type has been changed to a new `InputAcceptType` monoid to fix this.

### Longer Type Variables in Type Signatures

The type variables have been renamed to full words in the component / query / etc. type signatures. Maybe this will help, maybe not - feedback is welcome and appreciated!

### Migration to Spago

[Spago](https://github.com/purescript/spago) has emerged as the preferred dependency manager and build tool for PureScript. Halogen 5 -- both the library and the examples -- is now migrated entirely to Spago, with Bower used solely for publication.
