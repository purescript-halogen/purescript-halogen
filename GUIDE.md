# The Halogen guide

A Halogen application is represented by a tree of components. Each component definition describes how to render the component for a given state and provides a function that handles queries that can make changes to the state of the component and/or return details about the state.

A basic component:

``` purescript
import Prelude
import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E

-- | The state of the component
type State = { on :: Boolean }

-- | The query algebra for the component
data Query a
  = ToggleState a
  | GetState (Boolean -> a)

-- | The component definition
myComponent :: forall g. Component State Query g
myComponent = component { render, eval }
  where

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.h1_
          [ H.text "Toggle Button" ]
      , H.button
          [ E.onClick (E.input_ ToggleState) ]
          [ H.text (if state.on then "On" else "Off") ]
      ]

  eval :: Natural Query (ComponentDSL State Query g)
  eval (ToggleState next) = do
    modify (\state -> { on: not state.on })
    pure next
  eval (GetState continue) = do
    value <- gets _.on
    pure (continue value)
```

For a full runnable example take a look at the [template project](https://github.com/slamdata/purescript-halogen-template).

## Query algebras

The type for a component’s query values is represented by a type constructor of kind `* -> *` which is referred to as the _query algebra_ for the component. Using a type with this kind allows us to keep queries typed, so when using them to request a value the result type is known statically.

The constructors of a query algebra fall into two categories, _actions_ and _requests_. Actions are used to describe inputs that only modify the state of a component and return `unit`, whereas requests can modify the state as well as returning a value.

From the above example:

``` purescript
data Query a
  = ToggleState a
  | GetState (Boolean -> a)
```

Here `ToggleState` is an action and `GetState` is a request – the difference being the location of the query algebra’s type parameter. For actions the parameter is used as a value, for requests it appears in the return type of a function. This is how the previously mentioned typed queries work: when a request is formed using the `GetState` constructor, we know the result type must be a `Boolean` due to the way the query processor handles constructors of this shape.

The functions [`action`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Query#v:action) and [`request`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Query#v:request) in the [`Halogen.Query`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Query) module can be used to make queries when used with constructors of the appropriate type (for example, `action ToggleState` or `request GetState`).

## State

Each component has its own “private” state value. State cannot be modified during `render`, only during `eval`. When a component needs to make part of its state available externally it can do so by providing a request constructor in the query algebra (like `GetState` in the previous example).

## Component definitions

Components are constructed from a “spec“ record with the `component` function:

``` purescript
type ComponentSpec s f g =
  { render :: s -> ComponentHTML f
  , eval :: Natural f (ComponentDSL s f g)
  }

component :: forall s f g. ComponentSpec s f g -> Component s f g
```

- `s` is the component’s state.
- `f` is the component’s query algebra.
- `g` is a functor integrated into the component’s query algebra that allows embedding of external DSLs or handling of effects – often this will be `Aff`, or if the component interactions have no non-state effects this can be left polymorphic.

## Rendering

A `render` function takes the component’s current state value and returns a value constructed using Halogen’s type safe `HTML` DSL, with the ability for elements in the rendered HTML to send actions back to the component, using the query algebra `f`. `ComponentHTML` is a type alias that specialises the `HTML` type for use in components:

``` purescript
type ComponentHTML f = HTML Void (f Unit)
```

When building `HTML` values there are two options for modules that provide the standard HTML tags: [`Halogen.HTML`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.HTML) and [`Halogen.HTML.Indexed`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.HTML.Indexed). Use of the `Indexed` variety is recommended as this has a greater level of type safety and can aid type-directed programming.

### Event listeners

The `HTML` DSL allows event listeners to be set up in a declarative way, as demonstrated in the basic component example, and replicated here:

``` purescript
E.onClick (E.input_ ToggleState)
```

Functions for all standard HTML event types are provided by [`Halogen.HTML.Events`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.HTML.Events), and as with the elements there is a [`Halogen.HTML.Events.Indexed`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.HTML.Events.Indexed) variety that is recommended as the standard choice.

These modules also provides two functions, [`input`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.HTML.Events#v:input) and [`input_`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.HTML.Events#v:input_), which are used to turn query algebra constructors into actions for `eval`:

- `input` is for constructors where an additional value is expected, provided by reading some value from the event.
- `input_` is for constructors that require no additional values.

`input` is often useful when combined with the form-specific event helpers like [`onChecked`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.HTML.Events.Forms#v:onChecked) and [`onValueInput`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.HTML.Events.Forms#v:onValueInput):

``` purescript
import Halogen.HTML.Events.Indexed as E

data ExampleQuery a
  = SetOption Boolean a
  | SetText String a

-- Then when used elsewhere during HTML rendering:
E.onChecked (E.input SetOption)
E.onValueInput (E.input SetText)
```

It is also possible to declare an event listener that makes use of the `preventDefault`, `stopPropagation`, and `stopImmediatePropagation` event methods, at the same time as sending inputs to `eval`. To do this, the `input` and `input_` functions are omitted, and instead the listener should look something like this:

``` purescript
import Data.Functor (($>))
import Halogen.HTML.Events.Handler as EH

E.onClick (\_ -> EH.preventDefault $> Just (action ToggleState))
```

These functions from [`Halogen.HTML.Events.Handler`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.HTML.Events.Handler) can also be chained together, either as “statements” in a `do`, or with the `Apply` operator:

``` purescript
import Control.Apply ((*>))

E.onClick (\_ -> EH.preventDefault *> EH.stopPropagation $> Just (action ToggleState))
```

Note that in the above cases we’re not using the `input` or `input_` helpers but instead are constructing the query using [`action`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Query#v:action), and returning it in a `Just`. The `Maybe` here is so that we have the option to return `Nothing` sometimes, so we do not have to always raise a query in response to an event.

## Evaluating queries

The types involved in a component’s `eval` function:

``` purescript
eval :: Natural f (ComponentDSL s f g)
type ComponentDSL s f g = Free (HalogenF s f g)
```

`Natural` is a type synonym for a natural transformation (`forall a. f a -> g a`). The use of a natural transformation here is what give us the ability to have typed queries, as if we apply a value `f Boolean` to a `Natural f g`, the result has to be a `g Boolean`.

Evaluating an action query will look something like this:

``` purescript
eval (ToggleState next) = do
  -- somehow modify the state
  pure next
```

`next` is the `a` value in the algebra, and as mentioned previously it will always be `Unit`-typed for an action, but the typechecker can’t know this here so we are required to end actions by returning `next` rather than `unit`.

Request query cases will look something like this:

``` purescript
eval (GetState continue) = do
  -- get some result value from the state
  pure (continue result)
```

Here `continue` is the `Boolean -> a` function we defined, so the result value for `eval` here is produced by applying the function to a value of the expected type.

### Eval’s free monad

The `Free (HalogenF s f g) _` that `eval` functions operate in allow us to perform various actions using the `HalogenF` algebra, such as manipulating the state of the current component:

- [`get`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Query#v:get) retrieves the entire current state value
- [`gets f`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Query#v:gets) uses `f` to map the state value, generally used to extract a part of the state
- [`modify f`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Query#v:modify) uses `f` to update the stored state value
- [`set`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Query#v:set) overwrites the entire current state value.

There is also the ability to [`subscribe`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Query#v:subscribe) to “event sources”. This was introduced to allow subscriptions to event handlers and callbacks for 3rd party components (see the section [“Subscriptions and event sources”](#subscriptions-and-event-sources) for more details), but it is also possible to construct event sources to allow for some tricks where components need to send queries to themselves, however this latter use case is beyond the scope of this guide for now.

Finally, values of type `g` can be lifted into `HalogenF` to allow us to perform operations that lie outside of the component itself – generally `g` is `Aff`, allowing us to perform effectful and asynchronous operations such as AJAX requests.

Another option for `g` is to use a `Free` monad here with an algebra that encapsulate all of the effectful actions the component needs to perform, and then this can later be interpreted as `Aff`. There is a function provided for this, called [`interpret`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Component#v:interpret), that allows a component’s `g` value to be transformed.

### Non-state effects

One of the most common non-state effect for a component is to [make requests via AJAX](examples/ajax), and the principle is similar for any usage of any `Aff` style function.

Here’s the `eval` function from the AJAX example:

``` purescript
eval :: Natural Query (ComponentDSL State Query (Aff (AppEffects eff)))
eval (SetCode code next) = --- ... snip ...
eval (MakeRequest code next) = do
  modify (_ { busy = true })
  result <- fromAff (fetchJS code)
  modify (_ { busy = false, result = Just result })
  pure next

fetchJS :: forall eff. String -> Aff (ajax :: AJAX | eff) String
fetchJS code = -- ... make request ...
```

`fetchJS` is a normal `Aff`-returning function. To make use of it in a `Free (HalogenF s f g)` context we use [`fromAff`](https://pursuit.purescript.org/packages/purescript-aff-free/0.1.1/docs/Control.Monad.Aff.Free) to lift it into the right place.

Using `Aff` for a component’s `g` means it also inherits the convenience of `Aff`’s async handling behaviour – that is to say we need no explicit callbacks while waiting for async results, in the above example the second `modify` will not be executed until we have the `result` value.

If we want to use an `Eff` based function there is also a [`fromEff`](https://pursuit.purescript.org/packages/purescript-aff-free/0.1.1/docs/Control.Monad.Aff.Free#v:fromEff) helper.

If `g` is not `Aff` then lifting values into it is performed with the [`liftH`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Query#v:liftH) function provided by Halogen. There is an example of this in the [the “interpret” example](examples/interpret) where `g` is another `Free` monad that is later interpreted as `Aff`.

## The driver

To render our component (or tree of components) on the page we need to pass it to the [`runUI`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Driver#v:runUI) function:

``` purescript
runUI
  :: forall s f eff
   . Component s f (Aff (HalogenEffects eff))
  -> s
  -> HTMLElement
  -> Aff (HalogenEffects eff) (Driver f eff)
```

This takes our component, its initial state, and a HTML element to attach the rendered component to, and then returns a driver function via `Aff`. The driver function is a mechanism for querying the component “from the outside” – that is to say, send queries that don’t originate within the Halogen component structure.

``` purescript
type Driver f eff = Natural f (Aff (HalogenEffects eff))
```

The purpose of the driver function is to allow us to extract information from the application state, or more commonly, to do things like change the application state in response to changes in the URL using a routing library.

The [`Halogen.Util`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Util) module provides a collection of convenience functions for running Halogen components such as [`awaitBody`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Util#v:awaitBody) and [`runHalogenAff`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.Util#v:runHalogenAff). A basic `main` for a component using these functions might look something like this:

``` purescript
main :: Eff (HalogenEffects ()) Unit
main = runHalogenAff do
  body <- awaitBody
  driver <- runUI ui initialState body
  -- ... snip ...
```

The returned `driver` function then allows us to send actions to the component:

``` purescript
driver (action ToggleState)
```

Or request information from the component:

``` purescript
isToggled <- driver (request GetState)
```

[See the “counter” example](examples/counter) for an illustration of driving the app with an external timer.

## Child components

So far the examples have only concerned a single component, however this will only take us so far before the state and query algebra become unmanageable. The solution to this is to break our app into child components that can then be composed together.

### Parent components

A component that can contain other components is constructed with the `parentComponent` function:

``` purescript
type ParentComponentSpec s s' f f' g p =
  { render :: s -> ParentHTML s' f f' g p
  , eval :: Natural f (ParentDSL s s' f f' g p)
  , peek :: forall x. Maybe (ChildF p f' x -> ParentDSL s s' f f' g p Unit)
  }

parentComponent
  :: forall s s' f f' g p
   . (Functor g, Ord p)
  => ParentComponentSpec s s' f f' g p
  -> Component (ParentState s s' f f' g p) (ParentQuery f f' p) g
```

The types here may appear a little intimidating but aren’t really any more complicated than with the standard `component`:

- `s` is the component’s own state type, `s'` is the state type for child components
- `f` is the component’s own query algebra type, `f'` is the query algebra type for child components
- `g` is a functor integrated into the component’s query algebra for extended effect handling, and must be the same for both parent and children for them to be composable

`p` is the only new parameter here and is used to specify the value that will be used as the “slot address” which allows queries to be sent to specific child components.

`ParentHTML` and `ParentDSL` are variations on the previously described `ComponentHTML` and `ComponentDSL` synonyms but do not alter the way the `render` or `eval` functions are defined. These differ from the non-`Parent` synonyms by swapping out the `Void` in `HTML` for `SlotConstructor ...` and the `g` of `ComponentDSL` becomes `QueryF ...`. These changes allow us to perfom the necessary plumbing when installing child components into a parent.

Defining synonyms for the combination state and query algebra types is recommended when setting up parent components. Taking [the “components” example](examples/components) for instance, we define `StateP` and `QueryP` synonyms for the component we’re constructing:

``` purescript
type StateP g = ParentState State TickState Query TickQuery g TickSlot
type QueryP = Coproduct Query (ChildF TickSlot TickQuery)

ui :: forall g. Functor g => Component (StateP g) QueryP g
```

The `P` here stands for “prime”. Currently PureScript does not allow types to use the prime symbol, so we use `StateP` instead of `State'`. It could also stand for “parent”, so thinking of it either way works here!

Defining synonyms like these becomes especially important if the component is going to be installed inside yet another component – the types only ever have to refer “one level down” and unreadable deeply nested types are avoided.

### Slots

Slot address values will usually be a `newtype` wrapper around some kind of value that can be used as an identifier, such as an `Int` index or `String` name.

Slot address types requires an `Ord` instance to be defined for them to be usable within a component. These instances can be derived by the PureScript compiler, for example, from [the “components” example](examples/components):

``` purescript
import Data.Generic (Generic, gEq, gCompare)

newtype TickSlot = TickSlot String

derive instance eqTickSlot :: Eq TickSlot
derive instance ordTickSlot :: Ord TickSlot
```

Slot values can then be inserted into the `HTML` for the parent component using the [`slot`](https://pursuit.purescript.org/packages/purescript-halogen/0.6.0/docs/Halogen.HTML#v:slot) smart constructor:

``` purescript
render :: State -> ParentHTML TickState Query TickQuery g TickSlot
render st =
  H.div_
    [ H.slot (TickSlot "A") \_ -> { component: ticker, initialState: TickState 100 }
    , H.slot (TickSlot "B") \_ -> { component: ticker, initialState: TickState 0 }
         -- ... snip ...
    ]
```

This simple case illustrates a static child component setup, but dynamic structures can also be created by mapping a function that returns slot values over part of the component’s state.

The other argument passed to `slot` here is a thunk (a function that accepts a `unit` argument, used so the inner value is only evaluted as is necessary) that specifies which component should be used in the slot, and what value to use for its initial state.

The thunk is only evaluated when the slot first appears in the `HTML`, so modifying `initialState` will have no effect on repeat renders – to alter the state of the child component requires the use of queries.

**Note**: Using non-unique slot address values within a `HTML` structure is strongly discouraged, doing so may have unexpected effects if the components inserted into the slots have interactive elements or are “3rd party components”.

### Querying children

The `eval` function of a parent component has access to an additional combinator: `query`. This allows a parent component to query its children, using a slot address value of the child:

``` purescript
eval :: Natural Query (ParentDSL State TickState Query TickQuery g TickSlot)
eval (ReadTicks next) = do
  a <- query (TickSlot "A") (request GetTick)
  b <- query (TickSlot "B") (request GetTick)
  modify (\_ -> { tickA: a, tickB: b })
  pure next
```

So here we’re processing a `ReadTicks` action, and in response to that making requests of the child components in slots `TickSlot "A"` and `TickSlot "B"` in order to find some information about their states.

As we have no guarantee that a particular slot value is currently in the rendered `HTML` structure the `query` function returns `Maybe` values in response to queries. In the above example this is okay as the `tickA` and `tickB` values stored in the state are typed as `Maybe Int`, but sometimes we may have to do more work to handle query “failures”.

### Peeking

The type of components we’ve described so far only support communication in one way: top-down, from parent to children. Sometimes this isn’t powerful enough to model our user interfaces. For example: if we have a list component and an item component with a delete button, how do we tell the parent to delete the item when the button is clicked?

Allowing children to talk directly to parents would make composition and encapsulation of components problematic, so the solution Halogen adopts is to allow parents to “peek” at the queries their children have received.

Note _have received_ here – the child component is guaranteed to have already processed the query, Halogen does not offer any kind of mechanism for a parent to interfere with queries that are going to its children, only observe them after the fact.

[The TODO example](examples/todo) makes use of this technique to allow tasks to “delete themselves” from the list they reside within. First we have a `Remove` constructor in the query algebra for the tasks:

``` purescript
data TaskQuery a
  = UpdateDescription String a
  | ToggleCompleted Boolean a
  | Remove a
  | IsCompleted (Boolean -> a)
```

This doesn’t actually do anything inside the task so implementing a case for it in the task’s `eval` function is trivial:

``` purescript
eval (Remove next) = pure next
```

The `peek` value required as part of the `ParentComponentSpec` is as follows:

``` purescript
peek :: forall x. Maybe (ChildF p f' x -> ParentDSL s s' f f' g p Unit)
```

`Maybe` is used here as not every component requires the peeking ability, in which case `Nothing` can be provided for `peek` in the spec.

The function is much like `eval`, but instead of using a natural transformation here we’re throwing away the type carried by the query algebra (`x` here) and always returning a `Unit` value. This is necessary so that we don’t have to match every possible case of the child’s query algebra.

Going back to [the TODO example](examples/todo), here’s the `peek` function for our task list:

``` purescript
peek :: forall x. ChildF TaskSlot TaskQuery x -> ParentDSL List Task ListQuery TaskQuery g TaskSlot Unit
peek (ChildF p q) = case q of
  Remove _ -> do
    wasComplete <- query p (request IsCompleted)
    when (fromMaybe false wasComplete) $ modify $ updateNumCompleted (`sub` 1)
    modify (removeTask p)
  ToggleCompleted b _ -> modify $ updateNumCompleted (if b then (+ 1) else (`sub` 1))
  _ -> pure unit
```

`peek` functions always accept a `ChildF p q` value – this allows us to see which child received the query (the `p` value above is the slot placeholder value) and the query that was processed (the `q` value).

Here we’re observing when the child has processed a `Remove` query, and when doing so we remove it from the parent’s state and also revise the count for the number of completed and pending tasks. We also observe `ToggleCompleted` queries and likewise update the completed task count accordingly.

This usage of `peek` illustrates another point about its usage: when observing `Remove` we send a further query to the child in question to check on its completion state, however this does not trigger an infinitely recursive `peek`: any queries to children made inside `peek` or `eval` are not re-observed by `peek`.

### Multiple types of child component

Another common case encountered is the need to have different types of child component under the same parent. Halogen does support this, there is a demonstration in [the “multi component” example](examples/multi-component).

We use `Either`s for the differing state and slot types, and `Coproduct`s for the query algebra types. Once again, due to the increasing complexity of types, the use of type synonyms helps here. From the example:

``` purescript
type ChildState = Either StateA (Either StateB StateC)
type ChildQuery = Coproduct QueryA (Coproduct QueryB QueryC)
type ChildSlot = Either SlotA (Either SlotB SlotC)
```

We then need to define a “path through the types” that gets us access to the type in the appropriate position for each of the different child component types:

``` purescript
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR, (:>))

cpA :: ChildPath StateA ChildState QueryA ChildQuery SlotA ChildSlot
cpA = cpL

cpB :: ChildPath StateB ChildState QueryB ChildQuery SlotB ChildSlot
cpB = cpR :> cpL

cpC :: ChildPath StateC ChildState QueryC ChildQuery SlotC ChildSlot
cpC = cpR :> cpR
```

The `ChildPath s s' f f' p p'` type represents how we get to and from each of the types involved in a child component, where `s` is a child state and `s'` is the composite state we’re extracting it from or injecting it into.

It is possible to construct our own custom `ChildPath` values, but the most common approach is to make use of the `cpL` and `cpR` functions – these correspond roughly to “go left” and “go right” in the types. They “go the same way” with the state, query algebra, and slot type, so for these combinators to work the composite types constructed with `Either` and `Coproduct` need to follow the same structure in state, query algebra, and slot type.

The `:>` operator allows us to compose `ChildPath` values, so `cpR :> cpL` is “go left then go right” – this may seem backwards, but we’re starting at the deepest type and then going outwards. A more intuitive way of looking at the path is to read it and the corresponding type left-to-right:

``` purescript
cpR :> cpL
```

Would get us to the X in:

``` purescript
-- for state and slot types:
`Either _ (Either X _)`

-- for query algebras:
`Coproduct _ (Coproduct X _)`
```

Now we have our three components and “path” descriptions for them we can construct our parent component. This is only a little different to the standard case:

- When building the HTML, instead of using `H.slot`, we use an alternative version, `H.slot'` – this accepts a `ChildPath` value as well as the slot values.
- When querying children we use `query'` instead of `query`, once again the difference being that `query'` accepts a `ChildPath` argument first.

## Component lifecycles

Sometimes it is useful to be able to run actions when a component first comes into existence or when it is removed from the component tree. To achieve this there are two alternative component constructors, `lifecycleComponent` and `lifecycleParentComponent`.

These functions expect a spec record as usual, but the spec has two additional fields:

``` purescript
initializer :: Maybe (f Unit)
finalizer :: Maybe (f Unit)
```

These allow actions to be raised using the component’s query algebra during  its lifecycle.

The `initializer` fires immediately after rendering has completed for the whole component tree, after the new component appears in the tree for the first time. This ensures that any code we run in the `initializer` occurs when we know the DOM elements associated with the component exist in the actual DOM.

The `finalizer` fires after the component has been removed from the component tree. The action raised by a `finalizer` cannot be `peek`ed by the parent, as the component is no longer part of the component tree by the time it is evaluated. If a whole subtree of component is removed the finalizers for each removed component will fire, no matter how deep the component was in the hierarchy.

The [lifecycles example](examples/lifecycle) illustates the behaviour of initializers and finalizers by logging to the console as the actions are evaluated.

## Integrating 3rd party components

Halogen was designed with supporting “3rd party components” in mind, to allow apps to take advantage of existing JavaScript libraries that provide UI elements.

The [Ace editor example](examples/ace) illustrates the creation of a basic component wrapping a non-Halogen component.

### Creating a component

There is no special constructor for components that are going to wrap external functionality, although commonly the `lifecycleComponent` constructor will be preferred so that cleanup can occur in the `finalizer`.

Commonly a query algebra for a component of this kind will look something like this:

``` purescript
data Query a
  = SetElement (Maybe HTMLElement) a
  | Initialize a
  | Finalize a
  -- ... plus other component actions ...
```

`Initialize` and `Finalize` will be used to trigger cases in `eval` that setup and tear down the external component as necessary, and `SetElement` will be used to capture a reference to an element that has been rendered for the component, using the a special `ref` property in Halogen’s `HTML` DSL.

Even though we’re embedding an external component in our page we still need to render something for it, so that we have an element that we can attach our external component to. From the Ace editor example:

``` purescript
ace :: forall eff. Component AceState AceQuery (Aff (AceEffects eff))
ace = lifecycleComponent
    { render
    , eval
    , initializer: Just (action Initialize)
    , finalizer: Just (action Finalize)
    }
  where

  render :: AceState -> ComponentHTML AceQuery
  render = const $ H.div [ P.ref \el -> action (SetElement el) ] []

  eval :: Natural AceQuery (ComponentDSL AceState AceQuery (Aff (AceEffects eff)))
  eval = -- ... snip ...
```

So here `render` is a constant function returning an empty `div` with the `ref` property set, raising the `SetElement` query once the element is created.

**Important**: `ref` itself is not suitable for initializing and finalizing a component, as although it fires as soon as the element is created or removed there is no guarantee the element is actually in the DOM yet, and also it will not fire when a component has been removed entirely from the component tree. This is where the `SetElement` / `Initialize` / `Finalize` pattern comes in:

``` purescript
eval (SetElement el next) = do
  modify (_ { element = el })
  pure next
eval (Initialize next) = do
  el <- gets _.element
  case el of
    Nothing -> pure unit -- this case should never happen here, as refs fire before initializers even
    Just el' -> -- do something to initialize the component
  pure next
eval (Finalize next) = do
  -- clean up anything created during initialization
  pure next
```

### Subscriptions and event sources

Event and callback handling are both handled with the same mechanism: the `subscribe` function and an `EventSource` value.

An `EventSource` can be constructed with one of two helper functions:

``` purescript
eventSource
  :: forall eff a f
   . ((a -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
  -> (a -> Eff (avar :: AVAR | eff) (f Unit))
  -> EventSource f (Aff (avar :: AVAR | eff))

eventSource_
  :: forall eff f
   . (Eff (avar :: AVAR | eff) Unit -> Eff (avar :: AVAR | eff) Unit)
  -> Eff (avar :: AVAR | eff) (f Unit)
  -> EventSource f (Aff (avar :: AVAR | eff))
```

Both of these have a rather difficult looking type signatures but are fairly straightforward to use.

`eventSource` is for cases where we want to subscribe to an event/callback using a function that returns some value we’re interested in:

``` purescript
data Query a = TextCopied String a

eventSource (Editor.onCopy editor) \text -> do
  pure $ action (TextCopied text)
```

`eventSource_` is for cases where we have a callback that provides no return value:

``` purescript
eventSource_ (Session.onChange session) do
  text <- Editor.getValue editor
  pure $ action (ChangeText text)
```

We’re operating in `Eff` in the `do` in these examples so we can talk to the wrapped component, but then need to return a query as a result that will then be processed by `eval`.

### Other queries

Aside from some kind of “init query” the rest of the component’s query algebra can reflect actions and requests that should be made to the interior 3rd party component, it is then up to `eval` to translate these queries into the appropriate actions on the component:

``` purescript
eval (ChangeText text next) = do
  state <- gets _.editor
  case state of
    Nothing -> pure unit
    Just editor -> do
      current <- fromEff $ Editor.getValue editor
      when (text /= current) $ void $ fromEff $ Editor.setValue text Nothing editor
  pure next
```

This also illustrates why we want to store a reference to the 3rd party component in the state – we need to use it later to make evaluating the rest of the queries possible.
