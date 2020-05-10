# Routing Driver

This example demonstrates using `hashchange` events to drive the root component in a Halogen application.

This is a basic setup that knows nothing about routes and only observes changes to the URL. A real world app is likely to use a library like [`purescript-routing`](https://github.com/purescript-contrib/purescript-routing) to match routes.

## Building

You can build this example from the root of the Halogen project:

```sh
npm install
npm run example-driver-routing
```

This will bundle a runnable JS file, `example.js`, in the `examples/driver-routing/dist` directory. You can view the running application by opening the corresponding `index.html` file.
