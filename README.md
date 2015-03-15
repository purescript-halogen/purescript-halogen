# purescript-halogen

A declarative, type-safe UI library for PureScript.

## Getting Started

- Read the [Module documentation](MODULES.md).
- Try out the [example project](test/Main.purs).

`purescript-halogen` is a simple reactive UI library built on top of `virtual-dom`. It is based on the idea of _signal functions_.

A signal function is a state machine which consumes values of one input type, and yields values of an output type. In the case of our user interfaces, our signal functions will consume input events and yield HTML documents. The standard collection of instances (`Functor`, `Applicative`, etc.) allow us to compose these signal functions to create useful reactive documents.

The main idea in `purescript-halogen` is to separate the UI into the following components:

- A _pure_ signal function, which is responsible for responding to inputs, and generating HTML documents.
- A _handler function_, which is responsible for responding to DOM events, and initiating (possibly asynchronous) computations, which can result in new inputs.
- A _driver function_ which runs the signal function, using the handler function to turn DOM events into new inputs.

Halogen provides some general-purpose driver functions, and tools for building signal functions and handler functions.

In this way, we separate our effectful code from the view, which is a pure function of the UI state. All of the effects are pushed to the boundary of our application, in the _driver function_.

### A Pure Model

If the UI does not require any interaction with external components, and the only effects involved are DOM effects, we can simplify the model above.

In this case, the handler function will just take the DOM event, create a new input, and feed it back into the signal function. We can describe such a UI with the following type:

```purescript
type UI input = SF1 input (HTML input)
```

That is, we define a type of input messages, and create a signal function which consumes inputs and produces HTML documents which generate new inputs of the same type.

It is useful to see how this model can be used to create simple UIs before writing more interesting handler functions.

### A Simple State Machine

Here is a simple example. The `ui` function defines a signal function which responds to click events by incrementing a counter.

```purescript
data Input = Click

ui :: forall a. SF1 Input (HTML a Input)
ui = render <$> stateful 0 update
  where
  render :: Number -> HTML a Input
  render n = button [onclick \_ -> pure Click] [text (show n)]
  
  update :: Number -> Input -> Number
  update n Click = n + 1
  
main = do
  node <- runUI ui
  appendToBody node
```

Here, the user interface is represented as a signal function of type `SF1 Input (HTML Input)`. The type constructor `SF1` represents _non-empty_ signals, i.e. signals which have an initial output value. This just means that we have an initial HTML document to render when the application loads.

The `Applicative` instance is used to apply the `render` function (essentially the _view_) to a signal created using the `stateful` function (which acts as our _model_).

Note that the type `HTML Input` references the input event type. This means that our HTML documents can contain embedded functions which will generate events in response to user input. In this case, the `const Click` function is attached to the `onclick` handler of our button, so that our signal function will be run when the user clicks the button, causing the document to be updated.

### Handling Events

In the example above, the `button`'s `onclick` handler was bound to the `Click` message as follows:

```purescript
onclick \_ -> pure Click
```

Here, `pure` indicates that we are using an `Applicative` functor. The functor in question is the `EventHandler` functor, which can be used to perform common tasks related to events:

```purescript
onclick \_ -> preventDefault $> Click
```

Here, we use the `preventDefault` default function to call the `preventDefault` method on the event in the event handler. Other methods are supported, like `stopPropagation` and `stopImmediatePropagation`.

Generally, functions like `onclick` take arguments of type `Event fields -> EventHandler input`, where `Event fields` represents the DOM event type. That is, our HTML documents contain pure functions which generate inputs from DOM events.

### Mixins

Halogen provides "mixins" which can be used to add common functionality to our applications. Since signal functions allow us to give an entirely pure model of our view and state, a mixin is often as simple as a (higher-order) function, which modifies the types and functions described above, to add new functionality.

For example, the `UndoRedo` mixin allows us to add undo/redo functionality in a general way, by adding two new input messages (undo and redo) and modifying the state type passed to the `stateful` function to use a stack of previous states. 

### Composing UIs

#### `Functor`, `Applicative`

UIs can be composed in Halogen using the familiar `Functor` and `Applicative` type classes. For example, we used the `Functor` instance above to apply the `view` function to our state machine. 


The `Applicative` instance can also be used to compose user interfaces from smaller components which use the same input type:

```purescript
ui :: forall a. SF1 Input (HTML a Input)
ui = div_ <$> traverse [component1, component2, component3]
```

Signal functions have some other interesting type class instances:

#### `Category`

`SF` is a `Category`, and `SF1` is a `Semigroupoid`. This means that we can compose signal functions whose input and output types match, just like regular functions. In fact, you can think of `SF` as a function which maintains an internal state.

#### `Profunctor`

`SF` and `SF1` are both instances of the `Profunctor` class, which means that you can grow the output type or shrink the input type using `dimap`. This can be useful when trying to make two signal functions compatible so that they can be composed.

#### `Strong`, `Choice`

`SF` and `SF1` also have instances for the `Strong` and `Choice` type classes. These instances give us access to the following combinators:

```purescript
(***) :: forall i1 i2 o1 o2. SF i1 o1 -> SF i2 o2 -> SF (Tuple i1 i2) (Tuple o1 o2)
(+++) :: forall i1 i2 o1 o2. SF i1 o1 -> SF i2 o2 -> SF (Either i1 i2) (Either o1 o2)
(|||) :: forall i1 i2 o. SF i1 o -> SF i2 o -> SF (Either i1 i2) o
```

These combinators allow us to enlarge the input type in interesting ways, allowing us to construct graphs of signal functions, composing larger systems from smaller components.

### Custom Handler Functions

The pure model illustrated above completely ignored external components, but in real user interfaces, we need to be able to make calls to web services, local storage, etc., to determine the flow of data in our application.

We can use the `runUIEff` function to create interesting handler functions which interact with the world:

```purescript
runUIEff :: forall i a r eff. 
  SF1 i (HTML a (Either i r)) -> -- Pure signal function
  (a -> VTree) ->                -- Renderer for document placeholders
  Handler r i eff ->             -- Handler function
  Eff (HalogenEffects eff) (Tuple Node (Driver i eff))
```

The third argument to `runUIEff` is the handler function. Notice that the output type of the signal function has changed. Instead of generating inputs of type `i`, our HTML documents can now generate _requests_ of type `r`. The handler function will service requests and generate new inputs. Its type is:

```purescript
type Handler r i eff = r -> Driver i eff -> Eff (HalogenEffects eff) Unit
```

That is, a handler function takes a request and a driver function, and runs some asynchronous computation which will provide inputs to the driver function as they become available.

For example, the handler function might respond to requests by using the `purescript-affjax` library to make asynchronous AJAX calls, embedding the response content in an input message.

### Using Driver Functions 

The driver function generated by `runUIEff` simply passes its input to the current signal function, updating the internal state of the system, and eventually, the DOM.

In the pure model, all inputs to the driver function come from the DOM itself, but it is possible to "drive" the system externally by providing additional inputs. For example, we might use a timer to provide a tick input every second:

```purescript
main = do
  Tuple node driver <- runUIEff ui absurd handler
  appendToBody node
  setInterval 1000 $ driver Tick
```

### Placeholders

The first type argument of the `HTML` type constructor is used to create _placeholders_. Placeholders are used to embed third-party components in the user interface.

Since the signal function is pure, we have to handle placeholders outside the signal function at the top-level. The second argument to `runUIEff` is a function of type `a -> VTree`. This is the function used to render placeholders in the document.

Placeholders are only needed when using third-party components. Usually, the type variable `a` will be instantiated to the `Void` type, and the `absurd` function can be used as the rendering function.
