# Interpret

This example illustrates using `ReaderT` with `Aff` as the effect type for a component, and then interpreting this back down to `Aff` so it can be run as a normal component. This can be used, for example, to implement a read-only configuration for your application, or a global state if you store a mutable reference.

For an example application which uses `ReaderT` with `Aff` to implement a global state, see [Real World Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld/).

## Building

You can build this example from the root of the Halogen project:

```sh
npm install
npm run example-interpret
```

This will bundle a runnable JS file, `example.js`, in the `examples/interpret/dist` directory. You can view the running application by opening the corresponding `index.html` file.
