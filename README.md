# purescript-halogen

[![Latest release](http://img.shields.io/bower/v/purescript-halogen.svg)](https://github.com/slamdata/purescript-halogen/releases)
[![Build Status](https://travis-ci.org/slamdata/purescript-halogen.svg?branch=master)](https://travis-ci.org/slamdata/purescript-halogen)
[![Dependency Status](https://www.versioneye.com/user/projects/5620cabf36d0ab00210009a3/badge.svg?style=flat)](https://www.versioneye.com/user/projects/5620cabf36d0ab00210009a3)

A declarative, type-safe UI library for PureScript.

## Getting Started

- Read the [introduction](#introduction)
- Browse the [module documentation](http://pursuit.purescript.org/packages/purescript-halogen).
- Take a look at some examples:
  - [Basic counter](examples/counter/)
  - [AJAX requests](examples/ajax/)
  - [TODO list](examples/todo/)
  - [Ace editor](examples/ace/)

`purescript-halogen` uses the `virtual-dom` library as a CommonJS dependency. To set up `virtual-dom` in your project, it is recommended that you:

- Install `virtual-dom` as an NPM dependency in your `project.json` file.
- Use `psc` with `psc-bundle` and then `webpack` or `browserify` to build and link the `virtual-dom` source code into a JS bundle for use in the web browser. See a `gulpfile.js` in one of the example directories for a build setup using `webpack`.

## Introduction

A Halogen application is represented by a tree of components. Each component definition describes how to render the component for a given state and provides a function that handles queries that can make changes to the state of the component and/or return details about the state.

A basic component:

``` purescript
import Prelude
import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

-- | The state of the component
type State = { on :: Boolean }

-- | The query algebra for the component
data Query a
  = ToggleState a
  | GetState (Boolean -> a)

-- | The component definition
myComponent :: forall g. (Functor g) => Component State Query g
myComponent = component render eval
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

### Query algebras

The type for a component’s query values is represented by a type constructor of kind `* -> *` which is referred to as the _query algebra_ for the component. Using a type with this kind allows us to keep queries typed, so when using them to request a value the result type is known statically.

The constructors of a query algebra fall into two categories, _actions_ and _requests_. Actions are used to describe inputs that only modify the state of a component and return `unit`, whereas requests can modify the state as well as returning a value.

From the above example:

``` purescript
data Query a
  = ToggleState a
  | GetState (Boolean -> a)
```

Here `ToggleState` is an action and `GetState` is a request – the difference being the location of the query algebra’s type parameter. For actions the parameter is used as a value, for requests it appears in the return type of a function. This is how the previously mentioned typed queries work: when a request is formed using the `GetState` constructor, we know the result type must be a `Boolean` due to the way the query processor handles constructors of this shape.

The functions [`action`](docs/Halogen/Query.md#action-1) and [`request`](docs/Halogen/Query.md#request-1) in the [`Halogen.Query`](docs/Halogen/Query.md) module can be used to make queries when used with constructors of the appropriate type (for example, `action ToggleState` or `request GetState`).

### State

Each component has its own “private” state value. State cannot be modified during `render`, only during `eval`. When a component needs to make part of its state available externally it can do so by providing a request constructor in the query algebra (like `GetState` in the previous example).

### Component definitions

The basic component constructor:

``` purescript
component :: forall s f g. Render s f -> Eval f s f g -> Component s f g
```

The `render` and `eval` functions will be covered below, but first an explanation of the types variables:

- `s` is the component’s state.
- `f` is the component’s query algebra.
- `g` is a functor integrated into the component’s query algebra that allows embedding of external DSLs or handling of effects - often this will be `Aff`, or if the component interactions have no non-state effects this can be left polymorphic.

### Rendering

The types involved in a component’s `render` function:

``` purescript
type Render s f = s -> ComponentHTML f
type ComponentHTML f = HTML Void (f Unit)
```

A `render` function takes the component’s current state value and returns a value constructed using Halogen’s type safe `HTML` DSL, with the ability for elements in the rendered HTML to send actions back to the component, using the query algebra `f`.

When building `HTML` values there are two options for modules that provide the standard HTML tags: [`Halogen.HTML`](docs/Halogen/HTML.md) and [`Halogen.HTML.Indexed`](docs/Halogen/HTML/Indexed.md). It is recommended to use the `Indexed` variety as this has a greater level of type safety and can aid type-directed programming.

#### Event listeners

The `HTML` DSL allows event listeners to be set up in a declarative way, as demonstrated in the basic component example, and replicated here:

``` purescript
E.onClick (E.input_ ToggleState)
```

Functions for all standard HTML event types are provided by [`Halogen.HTML.Events`](docs/Halogen/HTML/Events.md), and as with the elements there is a [`Halogen.HTML.Events.Indexed`](docs/Halogen/HTML/Events/Indexed.md) variety that is recommended as the standard choice.

These modules also provides two functions, [`input`](docs/Halogen/HTML/Events.md#input) and [`input_`](docs/Halogen/HTML/Events.md#input_), which are used to turn query algebra constructors into actions for `eval`:

- `input` is for constructors where an additional value is expected, provided by reading some value from the event.
- `input_` is for constructors that require no additional values.

`input` is often useful when combined with the form-specific event helpers like [`onChecked`](docs/Halogen/HTML/Events/Forms.md#onchecked) and [`onValueInput`](docs/Halogen/HTML/Events/Forms.md#onvalueinput):

``` purescript
import qualified Halogen.HTML.Events.Indexed as E

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
import qualified Halogen.HTML.Events.Handler as EH

E.onClick (\_ -> EH.preventDefault $> action ToggleState)
```

These functions from [`Halogen.HTML.Events.Handler`](docs/Halogen/HTML/Events/Handler.md) can also be chained together, either as “statements” in a `do`, or with the `Apply` operator:

``` purescript
import Control.Apply ((*>))

E.onClick (\_ -> EH.preventDefault *> EH.stopPropagation $> action ToggleState)
```

Note that in the above cases we use [`action`](docs/Halogen/Query.md#action-1) to construct the query when not using the `input` or `input_` helpers.

### Evaluating queries

The types involved in a component’s `eval` function:

``` purescript
type Eval i s f g = Natural i (ComponentDSL s f g)
type ComponentDSL s f g = Free (HalogenF s f g)
```

Where `Natural` is a type synonym for a natural transformation (`forall a. f a -> g a`). The use of a natural transformation here is what give us the ability to have typed queries, as if we apply a value `f Boolean` to a `Natural f g`, the result has to be a `g Boolean`.

The `i` parameter is used to indicate the type of values the `eval` function is handling. In the majority of cases this will be `f`, but there are some occasions where it is useful to use the type synonym without being restricted to having `f` as the input (such as when `f` is a `Coproduct x y` and you want to define `evalX` and `evalY` functions separately).

Action query cases will look something like this:

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

#### Eval’s free monad

The `Free (HalogenF s f g) _` that `eval` functions return allow us to perform state updates for the current component, make use of the monad `g`, and subscribe to event listeners (this latter case is an advanced use case when [building widgets](#widgets-3rd-party-components) – the declarative listeners in the `HTML` DSL should be used where possible).

[`HalogenF`](docs/Halogen/Query.md#HalogenF) is actually a composite of 3 separate functors (algebras) which provide the different abilities just mentioned. The state-based algebra is defined in [`Halogen.Query.StateF`](docs/Halogen/Query/StateF.md) and [`Halogen.Query`](docs/Halogen/Query.md) provides an interface much like that provided by the standard `State` monad:

- [`get`](docs/Halogen/Query.md#get) retrieves the entire current state value
- [`gets f`](docs/Halogen/Query.md#gets) uses `f` to map the state value, generally used to extract a part of the state
- [`modify f`](docs/Halogen/Query.md#modify) uses `f` to update the stored state value

The second algebra is for event subscriptions, defined in [`Halogen.Query.SubscribeF`](docs/Halogen/Query/SubscribeF.md), and will be explained in the [section on widgets](widgets-3rd-party-components).

The third algebra is the `g` inherited from the component definition. This is often `Aff` and allows us to encapsulate any other effects a component might need. Another option is to use a `Free` monad here that encapsulates all the effectful actions the UI may need, that later is interpreted as `Aff` using [`Halogen.Component.interpret`](docs/Halogen/Component.md#interpret).

#### Non-state effects

One of the most common non-state effect for a component is to [make requests via AJAX](examples/ajax), and the principle is similar for any usage of any `Aff` style function.

Here’s the `eval` function from the AJAX example:

``` purescript
eval :: forall eff. Eval Query State Query (Aff (ajax :: AJAX | eff))
eval (MakeRequest input next) = do
  modify (_ { busy = true })
  result <- liftAff' (runRequest input)
  modify (_ { busy = false, result = Just result })
  pure next

runRequest :: forall eff. String -> Aff (ajax :: AJAX | eff) String
runRequest input = -- ... make request ...
```

`runRequest` is a normal `Aff`-returning function. To make use of it in a `Free (HalogenF s f g)` context we use the [`liftAff'`](docs/Halogen/Query.md#liftaff) helper function to lift it into the right place in the `Free`.

Using `Aff` for a component’s `g` means it also inherits the convenience of `Aff`’s async handling behaviour – that is to say we need no explicit callbacks while waiting for async results, in the above example the second [`modify`](docs/Halogen/Query.md#modify) will not be executed until we have the `result` value.

If we want to use an `Eff` based function there is also a [`liftEff'`](docs/Halogen/Query.md#lifteff) helper. [`liftAff'`](docs/Halogen/Query.md#liftaff) makes use of a `MonadAff` constraint and [`liftEff'`](docs/Halogen/Query.md#lifteff) uses the `MonadEff` constraint, so the `g` type does not have to strictly be `Aff` for these functions to work, any monad that provides `MonadAff` and `MonadEff` instances will suffice.

There is a third helper function of a similar nature, [`liftH`](docs/Halogen/Query.md#lifth). This is a generic way of lifting a `g` value into `Free (HalogenF s f g)`. It is the basis for [`liftAff'`](docs/Halogen/Query.md#liftaff) and [`liftEff'`](docs/Halogen/Query.md#lifteff), but also can be useful in its own right if `g` is a another `Free` monad that will later be interpreted into `Aff`. [See the “interpret” example](examples/interpret) for an illustration of this.

### The driver

To render our component (or tree of components) on the page we need to pass it to the [`runUI`](docs/Halogen/Driver.md#runui) function in [`Halogen.Driver`](docs/Halogen/Driver.md):

``` purescript
runUI
  :: forall s f eff
   . Component s f (Aff (HalogenEffects eff))
  -> s
  -> Aff (HalogenEffects eff) { node :: HTMLElement, driver :: Driver f eff }
```

What this does is take our component and its initial state, and then returns an HTML element and driver function via `Aff`. The HTML element is the node created for our component (so we can attach it to the DOM wherever we please), and the driver function is a mechanism for querying the component “from the outside” – that is to say, send queries that don’t originate within the Halogen component structure.

The purpose of the driver function is to allow us to extract information from the application state, or more commonly, to do things like change the application state in response to changes in the URL using a routing library.

The [`Halogen.Util`](docs/Halogen/Util.md) module provides a convenience function [`appendToBody`](docs/Halogen/Util.md#appendtobody) to use in cases where the entire page is being handled by Halogen. Putting this together to make a runnable `main` function gives us something like this:

``` purescript
main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI ui initialState
  appendToBody app.node
```

The returned `driver` function allows us to send actions to the component:

``` purescript
app.driver (action ToggleState)
```

Or request information from the component:

``` purescript
isToggled <- app.driver (request GetState)
```

[See the “counter” example](examples/counter) for an illustration of driving the app with an external timer.

### Child components

So far the examples have only concerned a single component, however this will only get you so far before the state and query algebra become unmanageable. The solution to this is to break your app into child components that can then be composed together.

#### Parent components

A component that can contain other components is constructed with the `parentComponent` function rather than `component`:

``` purescript
parentComponent
  :: forall s s' f f' g p
   . (Functor g, Ord p)
  => RenderParent s s' f f' g p
  -> EvalParent f s s' f f' g p
  -> Component (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
```

The types here may appear a little intimidating but aren’t really any more complicated than with the standard `component`:

- `s` is the component’s own state type, `s'` is the state type for child components
- `f` is the component’s own query algebra type, `f'` is the query algebra type for child components
- `g` is a functor integrated into the component’s query algebra for extended effect handling, and must be the same for both parent and children for them to be composable

`p` is the only new parameter here and is used to specify the value that will be used as the “slot address” which allows queries to be sent to specific child components.

`RenderParent` and `EvalParent` are variations on the previously described `Render` and `Eval` synonyms but do not alter the way the functions are defined, they just require typing differently to support the behaviours of the parent component:

``` purescript
type RenderParent s s' f f' g p = s -> ParentHTML s' f f' g p
type ParentHTML s' f f' g p = HTML (SlotConstructor s' f' g p) (f Unit)

type EvalParent i s s' f f' g p = Natural i (ParentDSL s s' f f' g p)
type ParentDSL s s' f f' g p = ComponentDSL s f (QueryF s s' f f' g p)
```

These differ from the non-`Parent` synonyms by swapping out the `Void` in `HTML` for `SlotConstructor ...` and the `g` of `ComponentDSL` becomes `QueryF ...`. These changes allow us to perfom the necessary plumbing when installing child components into a parent.

When setting up parent components it is recommended to define your own type synonyms for the state and query algebra. Taking [the “components” example](examples/components) for instance, we define `StateP` and `QueryP` synonyms for the component we’re constructing:

``` purescript
type StateP g = InstalledState State TickState Query TickQuery g TickSlot
type QueryP = Coproduct Query (ChildF TickSlot TickQuery)

ui :: forall g. (Functor g) => Component (StateP g) QueryP g
```

Defining synonyms like these becomes especially important if the component is going to be installed inside yet another component – the types only ever have to refer “one level down” and unreadable deeply nested types are avoided.

#### Slots

Slot address values will usually be a `newtype` wrapper around some kind of value that can be used as an identifier, such as an `Int` index or `String` name.

Slot address types requires an `Ord` instance to be defined for them to be usable within a component. An easy way of doing this is taking advantage of PureScript’s generic deriving and the [`purescript-generics`](https://github.com/purescript/purescript-generics) library to generate a default instance, for example, from [the “components” example](examples/components):

``` purescript
import Data.Generics (Generic, gEq, gCompare)

newtype TickSlot = TickSlot String

derive instance genericTickSlot :: Generic TickSlot
instance eqTickSlot :: Eq TickSlot where eq = gEq
instance ordTickSlot :: Ord TickSlot where compare = gCompare
```

Slot values can then be inserted into the `HTML` for the parent component using the [`slot`](docs/Halogen/HTML.md#slot) smart constructor:

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

The other argument passed to `slot` here is a thunk (function that accepts a `unit` argument, used so the inner value is only evaluted as is necessary) that specifies which component should be used in the slot, and what value to use for its initial state.

The thunk is only evaluated when the slot first appears in the `HTML`, so modifying `initialState` will have no effect on repeat renders – to alter the state of the child component requires the use of queries.

**Note**: Using non-unique slot address values within a `HTML` structure is strongly discouraged, doing so may have unexpected effects if the components inserted into the slots have interactive elements or are “3rd party components”.

#### Querying children

The `eval` function of a parent component has access to an additional combinator: `query`. This allows a parent component to query its children, using a slot address value of the child:

``` purescript
eval :: EvalParent Query State TickState Query TickQuery g TickSlot
eval (ReadTicks next) = do
  a <- query (TickSlot "A") (request GetTick)
  b <- query (TickSlot "B") (request GetTick)
  modify (\_ -> { tickA: a, tickB: b })
  pure next
```

So here we’re processing a `ReadTicks` action, and in response to that making requests of the child components in slots `TickSlot "A"` and `TickSlot "B"` in order to find some information about their states.

As we have no guarantee that a particular slot value is currently in the rendered `HTML` structure the `query` function returns `Maybe` values in response to queries. In the above example this is okay as the `tickA` and `tickB` values stored in the state are typed as `Maybe Int`, but sometimes we may have to do more work to handle query “failures”.

#### Peeking

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

When constructing a parent component that supports peeking we use a variation on `parentComponent`:

``` purescript
parentComponent'
  :: forall s s' f f' g p
   . (Functor g, Ord p)
  => RenderParent s s' f f' g p
  -> EvalParent f s s' f f' g p
  -> Peek (ChildF p f') s s' f f' g p
  -> Component (InstalledState s s' f f' g p) (Coproduct f (ChildF p f')) g
```

The only change being the addition of the argument typed with `Peek`. This is another synonym for a function:

``` purescript
type Peek i s s' f f' g p p' = forall a. i a -> ParentDSL s s' f f' g p Unit
```

This is much like the `eval` function, but instead of using a natural transformation here we’re throwing away the input type and always returning a `Unit` value. This is necessary so that we don’t have to match every possible case of the child’s query algebra.

Going back to [the TODO example](examples/todo), here’s the `peek` function for our task list:

```
peek :: Peek State Task ListQuery TaskQuery g ListSlot p
peek (ChildF p q) = case q of
  Remove _ -> do
    wasComplete <- fromMaybe false <$> query p (request IsCompleted)
    when wasComplete $ modify $ updateNumCompleted (`sub` 1)
    modify (removeTask p)
  ToggleCompleted b _ -> modify $ updateNumCompleted (if b then (+ 1) else (`sub` 1))
  _ -> pure unit
```

`peek` functions always accept a `ChildF p q` value – this allows us to see which child received the query (the `p` value is the slot placeholder value) and the query that was processed (the `q value`).

Here we’re observing when the child has processed a `Remove` query, and when doing so we remove it from the parent’s state and also revise the count for the number of completed and pending tasks. We also observe `ToggleCompleted` queries and likewise update the completed task count accordingly.

This usage of `peek` illustrates another point about its usage: when observing `Remove` we a send a further query to the child in question to check on its completion state, however this does not trigger an infinitely recursive `peek`: any queries to children made inside `peek` or `eval` are not re-observed by `peek`.

#### Multiple types of child component

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

### Widgets (3rd party components)

Halogen was designed with supporting “3rd party components” in mind, to allow apps to take advantage of existing JavaScript libraries that provide UI components. We refer to a 3rd party component wrapped inside a Halogen component as a widget.

This is an area that may have room for improvement in the future but certainly is powerful enough to make useful widgets possible.

The [Ace editor example](examples/ace) illustrates the creation of a basic widget.

#### A widget component

A widget is constructed using the standard `component` function but taking advantage of some features that are not used when defining a normal component.

Even though we’re embedding an external component in our page we still need to render a `HTML` value, so the suggested way of doing this is to generate a single empty element such as a `div` and then attach the special `initializer` property to it to handle setup once the element has been rendered to the page. From the Ace example:

``` purescript
ace :: forall eff. String -> Component AceState AceQuery (Aff (AceEffects eff))
ace key = component render eval
  where

  render :: Render AceState AceQuery
  render = const $ H.div [ initializer ] []

  initializer :: Prop AceQuery
  initializer = ...

  eval :: Eval AceQuery AceState AceQuery (Aff (AceEffects eff))
  eval = ...
```

#### Initializers and finalizers

The `Initializer` property allows us to define an action that will run on the widget once the element it belongs to has been rendered to the DOM. From the Ace example:

``` purescript
initializer :: Prop AceQuery
initializer = P.initializer \el -> action (Init el)
```

There is also a `finalizer` property that works in a similar way to the `initializer` but runs when the rendered HTML element is being removed from the DOM instead, to allow any cleanup that may be required when removing the widget.

#### Initializing the widget

Now we have the ability to raise some kind of query in response to the element appearing in the DOM we need to define how to initialize our widget. Generally this will involve two things:

1. Calling some function to initialize the 3rd party component and then storing a reference to it in the widget’s state so we can refer to it again later
2. Subscribing to events or callbacks on the 3rd party component

Referring back to the Ace example again, that might look something like this:

``` purescript
eval (Init el next) = do

  -- Initialize the 3rd party component
  editor <- liftEff' $ Ace.editNode el Ace.ace

  -- Store the returned value
  modify _ { editor = Just editor }

  -- Subscribe to the Ace editors' onChange callback
  session <- liftEff' $ Editor.getSession editor
  subscribe $ eventSource_ (Session.onChange session) $ do
    text <- liftEff $ Editor.getValue editor
    pure $ action (ChangeText text)
  pure next
```

There will often be a lot of `liftEff'` usage in the `eval` function for a widget as interacting with the 3rd party component will be inherently effectful.

#### Subscriptions and event sources

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
  pure $ actionF (TextCopied text)
```

`eventSource'` is for cases where we have a callback that provides no return value:

``` purescript
eventSource_ (Session.onChange session) do
  text <- Editor.getValue editor
  pure $ actionF (ChangeText text)
```

We’re operating in `Eff` in the `do` in these examples so we can talk to the wrapped component, but then need to return a query as a result that will then be processed by the widget `eval`.

#### Widget queries

Aside from some kind of “init query” the rest of a widget’s query algebra can reflect actions and requests that should be made to the wrapped 3rd party component, it is then up to `eval` to translate these queries into the appropriate actions on the component:

``` purescript
eval (ChangeText text next) = do
  state <- gets _.editor
  case state of
    Nothing -> pure unit
    Just editor -> do
      current <- liftEff' $ Editor.getValue editor
      when (text /= current) $ void $ liftEff' $ Editor.setValue text Nothing editor
  pure next
```

This also illustrates why we want to store a reference to the 3rd party component in the state – we need to use it later to make evaluating the rest of the widget’s queries possible.
