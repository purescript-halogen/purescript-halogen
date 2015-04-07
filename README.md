# purescript-halogen

[![Build Status](https://travis-ci.org/slamdata/purescript-halogen.svg?branch=master)](https://travis-ci.org/slamdata/purescript-halogen)

A declarative, type-safe UI library for PureScript.

## Getting Started

- Read the [module documentation](docs/).
- Read the examples:
  - [TODO List](examples/todo/Main.purs)
  - [Counter](examples/counter/Main.purs)
  - [AJAX](examples/ajax/Main.purs)
  - [Ace Editor](examples/ace/Main.purs)

`purescript-halogen` is a simple reactive UI library built on top of `virtual-dom`. It aims to answer the following questions:

- How can we describe web pages as pure functions of application state? 
- How can we handle DOM events which modify the application state?
- How can we incorporate asynchronous requests (such as AJAX) into our solution?
- How can we handle third party components in a pure functional UI?
- How can we create reusable components which encompass all of the above?

Halogen answers these questions by building layers of abstraction using simple concepts like _signal functions_.

## Signals and Signal Functions

> How can we describe web pages as pure functions of application state?

There is a standard, elegant answer to this question: we can model documents as _signals_, or time-varying values. When the signal changes, we push changes to the DOM.

```purescript
type UI0 = Signal HTML
```

A reasonable `Signal` type constructor should implement type classes like `Functor` and `Applicative`, allowing us to compose signals and documents to create our application.

However, when we come to answer the second question, things are not quite so clear:

> How can we handle DOM events which modify the application state?

Our `Signal` generates documents over time in response to inputs from external sources, but those documents will contain elements which are capable of generating new inputs, which we would like in turn to influence our `Signal`. The model now contains a feedback loop.

Halogen's answer to this question is to move from signals to _signal functions_.

First, we modify our `HTML` type to contain the type of events we are interested in modeling:

```purescript
type UI1 input = Signal (HTML input)
```

Now our documents can supposedly generate events, but we still can't represent the feedback loop in which events modify the state of our signal. We can pass our generated inputs as an argument to our UI function, modeling them as an external signal:

```purescript
type UI2 input = Signal input -> Signal (HTML input)
```

Here, we assume the existence of some worker function which is capable of taking our specification of type `UI2 input`, and "tying the knot", passing our generated inputs back into the input signal.

Instead of defining a type `Signal` of signals, Halogen defines a type of _signal functions_, `SF`, which we can think of intuitively as a function like `UI2`. So our "signal function model" looks like:

```purescript
type UI3 input = SF input (HTML input)
```

We can think of a value of type `SF i o` as a state machine which receives an input of type `i` and yields values of type `o` at each step.

Signal functions have a pure model, so that we can separate our UI's description from the way in which it actually interacts with the world.

`SF` implements the usual set of type classes (`Functor`, `Apply`, `Applicative`), but also implements some new ones: `Category`, `Profunctor`, `Strong` and `Choice`, allowing us to compose signal functions in more interesting ways.

_Note_: in reality, Halogen distinguishes between signal functions (`SF`) and non-empty signal functions (`SF1`), but in practice this distinction is rarely important.

The `Halogen.Signal` module also provides some useful combinators, such as the `stateful` function, which can be used to build simple signal functions.

The TODO example provides a good overview of the signal function approach.

## Handling Asynchronous Requests

> How can we incorporate asynchronous requests (such as AJAX) into our solution?

With our signal function model, it is now simple to support things like AJAX which generate events asynchronously. We can wedge a monad `m` into our signal function as follows:

```purescript
type UI4 m input = SF input (HTML (m input)) 
```

This allows us to create documents which generate inputs in some effectful way. In practice, Halogen requires `m` to be the `Event` monad, defined in the `Halogen.HTML.Events.Monad` module. `Event` uses the `Aff` monad to support define asynchronous actions. `Aff` is defined in the `purescript-aff` library, and provides a simple AJAX API in the `affjax` library.

Separating the monad `m` into a type argument is useful, because we can choose a different monad with which to define our application (such as a `Free` monad), choosing to interpret that monad in different ways for different purposes (production, testing, etc.)

See the AJAX example for a demonstration of the `Event` monad.

## Third-Party Components

Halogen provides a simple way to incorporate third-party components into UIs. In the interest of keeping our signal function pure, we don't simply extend the `HTML` type with arbitrary DOM content, but instead provide the ability to add _placeholder_ nodes to a `HTML` document. We tag placeholders with a type argument `p`:

```purescript
type UI5 m p input = SF input (HTML p (m input)) 
```

In practice, Halogen chooses `p` to be the type `Widget eff input`, which describes a `virtual-dom` widget, but as with the type argument `m`, leaving things polymorphic has the advantage that we can choose to interpret our model in a different way for testing.

The `p` type parameter also gives us a way to "graft" HTML documents at a point which is marked by a placeholder. We can think of the type `HTML p a` as describing HTML documents with "holes" marked with values of type `p`.

See the Ace editor example for a demonstration of placeholders.

## Components

> How can we create reusable components which encompass all of the above?

The `Halogen` module defines the `runUI` function, which interprets our pure model of the UI, using `virtual-dom` to render it to the DOM and attach any necessary event handlers.

The `Component` type describes all of the data which is needed by `runUI`:

```purescript
data Component p m req res
```

There are several type parameters here:

- `p` and `m` have the same interpretation as for `HTML`. They represent placeholders and the effects with which we generate inputs respectively.
- `req` is the type of input messages, or _requests_.
- `res` is the type of output messages, or _responses_.

`Component` represents a reusable component, which can maintain its own internal state, use its own internal messages, perform asynchronous actions. Components present an API to the application by means of the `req` and `res` types.

It is easy to create a `Component` from a signal function, using the `component` function:

```purescript
component :: forall p m req res. (Functor m) => SF1 req (HTML p (m res)) -> Component p m req res
```

But components are more than just signal functions. They can send and receive internal messages, as demonstrated by the `component'` function:

```purescript
component' :: forall p m req res i. SF1 (Either i req) (HTML p (m (Either i res))) -> Component p m req res
``` 

Third party components can also be turned into `Component`s, using the `widget` function.

`Component` provides an instance for the `Profunctor` class, and some other useful combinators so that components can be composed to create useful applications.

The Ace editor example demonstrates how components can be composed.

## Example

Here is a simple example. The `ui` function defines a component which responds to click events by incrementing a counter.

```purescript
data Input = Click

ui :: forall p m. Component p m Input Input
ui = component (render <$> stateful 0 update)
  where
  render :: Number -> HTML p (m Input)
  render n = button [onclick $ input \_ -> Click] [ text (show n) ]
  
  update :: Number -> Input -> Number
  update n Click = n + 1
  
main = do
  Tuple node _ <- runUI ui
  -- Render the node to the DOM
```

Here, the user interface is represented as a signal function which is wrapped in a `Component`. Notice that the type signatures are actually quite simple. Because we are not using placeholders or effects, we can keep the type arguments `p` and `m` polymorphic.

## Handling Events

In the example above, the `button`'s `onclick` handler was bound to the `Click` message as follows:

```purescript
onclick $ input \_ -> Click
```

The `input` function is used to generate an input to the state machine without effects. It is implemented as a wrapper around `pure`.

In general, the `EventHandler` applicative functor is used to create event handlers. It supports operations on events such as `preventDefault`:

```purescript
onclick \_ -> preventDefault $> pure Click
```

Other methods are supported, like `stopPropagation` and `stopImmediatePropagation`.

Generally, functions like `onclick` take arguments of type `Event fields -> EventHandler input`, where `Event fields` represents the DOM event type. That is, our HTML documents contain pure functions which generate inputs from DOM events.

## Mixins

Halogen provides "mixins" which can be used to add common functionality to our applications. Since signal functions allow us to give an entirely pure model of our view and state, a mixin is often as simple as a (higher-order) function, which modifies the types and functions described above, to add new functionality.

For example, the `UndoRedo` mixin allows us to add undo/redo functionality in a general way, by adding two new input messages (undo and redo) and modifying the state type passed to the `stateful` function to use a stack of previous states. 

## Custom Handler Functions

The `runUI` action also returns a _driver function_ of type `Driver req eff`, which simply passes its input to the current signal function, updating the internal state of the system, and eventually, the DOM.

Usually, inputs to the driver function come from the DOM itself, but it is possible to "drive" the system externally by providing additional inputs. For example, we might use a timer to provide a tick input every second.

The Counter example gives a demonstration of using a driver function in this way.
