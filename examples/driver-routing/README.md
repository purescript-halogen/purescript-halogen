## Routing example

This example illustrates using a `hashchange` events to drive the main component in a Halogen application.

This is a basic setup that knows nothing about routes, just observes changes to the URL. A real world app might use something like [`purescript-routing`](https://github.com/slamdata/purescript-routing) to match routes, etc.

### Building

From the root of the Halogen project:

```
$ npm install
$ npm run example-driver-routing
```
