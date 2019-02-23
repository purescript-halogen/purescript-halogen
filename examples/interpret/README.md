## Interpreter example

This example illustrates using `ReaderT` with `Aff` as the effect type for a component, and then interpreting this back down to `Aff` so it can be run as a normal component.

### Building

From the root of the Halogen project:

```
$ npm install
$ bower install
$ npm run example-interpret
```

The code will be built as `example.js` in the `examples/interpret/dist` directory within the example, runnable by opening the corresponding `index.html`.
