# purescript-halogen

[![Latest release](http://img.shields.io/bower/v/purescript-halogen.svg)](https://github.com/slamdata/purescript-halogen/releases)
[![Build Status](https://travis-ci.org/slamdata/purescript-halogen.svg?branch=master)](https://travis-ci.org/slamdata/purescript-halogen)

A declarative, type-safe UI library for PureScript.

Please note, v0.5 is still under development so this README is not yet fully complete.

## Getting Started

- Read the [introduction](#introduction)
- Browse the [module documentation](http://pursuit.purescript.org/packages/purescript-halogen).
- Try out the examples:
  - [TODO List](examples/todo/)
  - [Counter](examples/counter/)
  - [AJAX](examples/ajax/)
  - [Ace Editor](examples/ace/)

`purescript-halogen` uses the `virtual-dom` library as a CommonJS dependency. To set up `virtual-dom` in your project, it is recommended that you:

- Install `virtual-dom` as an NPM dependency in your `project.json` file.
- Use `psc` with `psc-bundle` and then `webpack` or `browserify` to build and link the `virtual-dom` source code into a JS bundle for use in the web browser. See a `gulpfile.js` in one of the example directories for a build setup using `webpack`.

## Introduction

**NOTE**: Previous to v0.5 Halogen had a different architecture, [see here](https://github.com/slamdata/purescript-halogen/tree/30e8b2c7174a1eeda73f67cc42c739ca24a1e218) for the most recent commit of the previous design (however this will not compile with PureScript v0.7.3 or later).

A Halogen application is represented by a tree of components. Each component describes how it will be rendered for a given state value, and provides a function that handles "queries" that can make changes to the state of the component and also optionally return details about the current state.

A basic component with its state and query type definitions:

``` purescript
-- | The state of the application
newtype State = State { on :: Boolean }

-- | The input query algebra
data Input a
  = ToggleState a
  | GetState (Boolean -> a)

-- | The UI component
ui :: forall g p. (Functor g) => Component State Input g p
ui = component render eval
  where

  render :: Render State Input p
  render (State state) = H.div_
    [ H.button [ E.onClick (E.input_ ToggleState) ]
               [ H.text (if state.on then "On" else "Off") ]
    ]

  eval :: Eval Input State Input g
  eval (ToggleState next) = do
    modify (\(State state) -> State { on: not state.on })
    pure next
  eval (GetState k) = do
    value <- gets (\(State state) -> state.on)
    pure (k value)
```

### Query algebras

A type for a component's queries is represented by a type constructor of kind `* -> *` which is referred to as the _query algebra_ for the component. Using a type with this kind allows us to keep queries typed, so when using them to request a value the result type is known statically.

The constructors of a query algebra fall into two categories, _actions_ and _requests_. Actions are used to describe inputs that only modify the state of a component, whereas requests can modify the state and return a value in response too.

From the above example:

``` purescript
data Input a
  = ToggleState a
  | GetState (Boolean -> a)
```

Here `ToggleState` is an action and `GetState` is a request - the difference being the location of the query algebra's type parameter. For actions the parameter is used as a value, for requests it appears in the return type of a function. This how the the previously mentioned typed queries work - when a request is formed using the `GetState` constructor, we know the result type must be a `Boolean` due to the way the query processor handles constructors of this shape.

The functions `action` and `request` in the `Halogen.Query` module can be used to make queries when used with constructors of the appropriate shape (for example, `action ToggleState` or `request GetState`).

### State

Each component has its own "private" state value that can only be accessed from its `render` and `eval` functions. State cannot be modified during `render`ing, only during `eval`. When a component needs to make part of its state available externally it should do by providing a request constructor in the query algebra (like `GetState` in the previous example).

### Component definitions

The basic component constructor is typed as follows:

``` purescript
component :: forall s f g p. Render s f p -> Eval f s f g -> Component s f g p
```

The `render` and `eval` functions will be covered below, but first an explanation of the types variables:

- `s` is the component's state
- `f` is the component's query algebra
- `g` is the monad handling the component's non-state effects (in practice this is almost always `Aff`, but can be left as `g` if the component has no non-state effects)
- `p` is the type of placeholders within the component, used to specify "holes" into which child components can be installed (leaving this as `p` is recommended if the component is not using placeholders)

Placeholders will be explained more in depth in the [section on child components](#child-components).

### Rendering

The type for a component's `render` function is as follows:

``` purescript
type Render s f p = s -> HTML p (f Unit)
```

What this means is the function takes the state value `s` for the component and returns a value using Halogen's type safe `HTML` DSL, incorporating placeholders of type `p` in the `HTML`, and with the ability for UI elements in the rendered HTML to send _actions_ to the current component, using the query algebra `f`. Because the type `f Unit` is used here only action-style queries are allowed, no requests.

#### Event listeners

The HTML DSL allows event listeners to be set up in a declarative way, as demonstrated in the basic component example, and replicated here:

``` purescript
E.onClick (E.input_ ToggleState)
```

Functions for all standard HTML event types are provided by `Halogen.HTML.Events`, and some special cases are provided for form inputs in `Halogen.HTML.Events.Forms`.

`Halogen.HTML.Events` also provides two functions, `input` and `input_`, which are used to turn query algebra constructors into actions for `eval`:

- `input` is for constructors where an additional value is expected, provided by reading some value from the event.
- `input_` is for constructors that require no additional values.

`input` is often useful when combined with the form-specific event helpers like `onChecked` and `onValueInput`:

``` purescript
data ExampleInputs a
  = SetOption Boolean a
  | SetText String a

-- Then when used elsewhere during HTML rendering:
F.onChecked (E.input SetOption)
F.onValueInput (E.input SetText)
```

It is also possible to declare an event listeners that makes use of the `preventDefault`, `stopPropagation`, and `stopImmediatePropagation` event methods, at the same time as sending inputs to `eval`. To do this, the `input` and `input_` functions are omitted, and instead the listener should look something like this:

``` purescript
E.onClick (\_ -> EH.preventDefault $> action ToggleState)
```

`EH` is an alias for `Halogen.HTML.Events.Handler` here, which is where the `preventDefault` and similar functions are defined. They can also be chained together:

``` purescript
E.onClick (\_ -> EH.preventDefault *> EH.stopPropagation $> action ToggleState)
```

Note that in the above cases we use `action` from the `Halogen.Query` module to construct the query when not using the `input` or `input_` helpers.

### Evaluating queries

The type for a component's `eval` function is as follows:

``` purescript
type Eval i s f g = Natural i (Free (HalogenF s f g))
```

Where `Natural` is a type synonym for natural transformations: `forall a. f a -> g a`. The use of a natural transformation here is what give us the ability to have typed queries, as if we apply a value `f Boolean` to a `Natural f g`, the result has to be a `g Boolean`.

The `i` parameter is used to indicate the type of values the `eval` function is handling. In the majority of cases this will be `f`, but there are some cases where it is useful to be able to use the type synonym without being restricted to having `f` as the input (such as when `f` is a `Coproduct x y` and you might want to define `evalX` and `evalY` functions separately, and still make use of the `Eval` type synonym).

"Action-style" query cases usually look something like this:

``` purescript
eval (ToggleState next) = do
  -- somehow modify the state
  pure next
```

`next` is the `a` value in the algebra, and as discussed previously it will _always_ be `Unit`-typed for an action, but the typechecker doesn't know that so we are required to end actions by returning `next`.

"Request-style" cases will look something like this:

``` purescript
eval (GetState k) = do
  -- get some result value from the state
  pure (k result)
```

Here `k` is the `Boolean -> a` function we defined, so the `next` value is produced by passing a value into it. Once we've applied the function, we return the result of that and we're done!

#### Eval's free monad

The `Free (HalogenF s f g) _` that `eval` functions return allow us to perform state updates for the current component, make use of actions in the monad `g`, and subscribe to event listeners (this latter case is an advanced use case when [building widgets](widgets-3rd-party-components), the declarative listeners in the `HTML` DSL should be used where possible).

`HalogenF` is actually a composite (coproduct) of 3 separate functors which provide the different abilities just mentioned. The state-based actions are defined in `Halogen.Query.StateF` which provides an interface much like that provided by the standard `state` monad:

- `get` retrieves the entire current state value
- `gets f` uses `f` to map the state value, generally used to extract a part of the state 
- `modify f` uses `f` to update the stored state value 

The second functor is for event subscriptions, defined in `Halogen.Query.SubscribeF`, and will be explained in the [section on widgets](widgets-3rd-party-components).

The third functor is the `g` inherited from the component definition, as mentioned previously this is typically `Aff` and allows us to encapsulate any other effects a component might need.

#### Non-state effects

One of the most common non-state effect for a component is to [make requests via AJAX](examples/ajax). The principle is similar for any usage of an `Aff` style function so we'll use that as an example here:

``` purescript
eval :: forall eff. Eval Input State Input (Aff (ajax :: AJAX | eff))
eval (MakeRequest input next) = do
  modify (_ { busy = true })
  result <- liftFI (runRequest input)
  modify (_ { busy = false, result = Just result })
  pure next

runRequest :: forall eff. String -> Aff (ajax :: AJAX | eff) String
runRequest input = -- ... make request ...
```

As you can see, `runRequest` is just a function that returns a value in `Aff` the usual way. To make use of it in a `Free (HalogenF s f g)` context we use the `Control.Monad.Free.liftFI` function to "lift and inject" it into the right place in the `Free` monad.

Using `Aff` for a component's `g` means it also inherits the convenience of `Aff`'s async handling behaviour - that is to say we need no explicit callbacks or anything while waiting for async results, in the above example the second `modify` will not be executed until we have the `result` value. 

---

The following sections are still TODO:

### The driver

### Child components

#### Parent components
#### Installing
#### Peeking

### Widgets (3rd party components)

#### Initializers and finalizers
#### Event sources
