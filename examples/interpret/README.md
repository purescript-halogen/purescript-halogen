## Interpreter example

This example illustrates using a free monad as the component's `g` value and then later interpreting that into an `Aff` value before running the component.

### Building

From the root of the Halogen project:

```
$ npm install
$ npm run example-interpret
```

Or from the current directory:

```
$ npm install
$ bower install
$ npm run build
```

The code will be built as `example.js` in the `examples/interpret/dist` directory within the example, runnable by opening the corresponding `index.html`.
