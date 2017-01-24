## WebSocket driver example

This example illustrates using a WebSocket to drive the main component in a Halogen application. The visible result is unimpressive as we're just using an echo service and pushing in all the messages at the start, but the setup would be mostly the same for a real world case.

### Building

From the root of the Halogen project:

```
$ npm install
$ npm run example-driver-websockets
```

Or from the current directory:

```
$ npm install
$ bower install
$ npm run build
```

The code will be built as `example.js` in the `examples/driver-websockets/dist` directory within the example, runnable by opening the corresponding `index.html`.
