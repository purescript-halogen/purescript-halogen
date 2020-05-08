# WebSocket Driver

This example demonstrates using a WebSocket to drive the main component in a Halogen application.

The visible result is unimpressive as we're just using an echo service and pushing in all the messages at the start, but the setup would be mostly the same for a real world case.

## Building

You can build this example from the root of the Halogen project:

```sh
npm install
npm run example-driver-websockets
```

This will bundle a runnable JS file, `example.js`, in the `examples/driver-websockets/dist` directory. You can view the running application by opening the corresponding `index.html` file.
