# Improving Performance With Laziness

This example demonstrates how to improve performance by skipping expensive functions in rendering when their arguments are unchanged.

## Building

You can build this example from the root of the Halogen project:

```sh
npm install
npm run example-memoized-lazy
```

This will bundle a runnable JS file, `example.js`, in the `examples/memoized-lazy/dist` directory. You can view the running application by opening the corresponding `index.html` file.
