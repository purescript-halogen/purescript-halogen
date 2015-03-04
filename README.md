# purescript-halogen

A declarative, type-safe UI library for PureScript.

## Getting Started

- Read the [Module documentation](MODULES.md).
- Try out the [example project](test/Main.purs).

`purescript-halogen` is a simple reactive UI library built on top of `virtual-dom`. It is based on the idea of _signal functions_.

A signal function is a state machine which consumes values of one input type, and yields values of an output type. In the case of our user interfaces, our signal functions will consume input events and yield HTML documents. The standard collection of instances (`Functor`, `Applicative`, etc.) allow us to compose these signal functions to create useful reactive documents.

Here is a simple example.

```purescript
data Input = Click

ui :: forall eff. SF1 eff Input (HTML Input)
ui = render <$> stateful 0 (\n _ -> n + 1)
  where
  render :: Number -> HTML Input
  render n = button [onclick (const Click)] [text (show n)]
```

Here, the user interface is represented as a signal function of type `Signal1 eff Input (HTML Input)`. The type constructor `Signal1` represents _non-empty_ signals, i.e. signals which have an initial output value. This just means that we have an initial HTML document to render when the application loads.

The `Applicative` instance is used to apply the `render` function (essentially the _view_) to a signal created using the `stateful` function (which acts as our _model_).

Note that the type `HTML Input` references the input event type. This means that our HTML documents can contain embedded functions which will generate events in response to user input. In this case, the `const Click` function is attached to the `OnClick` handler of our button, so that our signal function will be run when the user clicks the button, causing the document to be updated.
