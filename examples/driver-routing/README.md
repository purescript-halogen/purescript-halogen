## Routing example

This example illustrates using a `hashchange` events to drive the main component in a Halogen application.

This is a basic setup that knows nothing about routes, just observes changes to the URL. A real world app might use something like [`purescript-routing`](https://github.com/slamdata/purescript-routing) to match routes, etc.

### Building

From the root of the Halogen project:

```
$ npm install
$ npm run example-driver-routing
```

Or from the current directory:

```
$ npm install
$ bower install
$ npm run build
```

The code will be built as `example.js` in the `examples/driver-routing/dist` directory within the example, runnable by opening the corresponding `index.html`.
