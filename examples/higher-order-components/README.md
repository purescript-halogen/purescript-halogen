# Higher order components example

This example illustrates how to define and use a higher-order component. A higher-order component is a function which receives a component, wraps it (typically adding some new functionality at the same time), and then returns the wrapped component.

In this example there is a container which has a button as a child, but the button is wrapped using the higher order component. The factory (higher-order component) in this example illustrates the following properties:

- Inputs to the inner component get lifted
- Messages from the inner component get lifted
- The inner component can be queried using the `liftQuery` helper function
- The generated component can interact with inner components, as long as their query type has an instance of the `CanSet` typeclass.

## Building

You can build this example from the root of the Halogen project:

```sh
npm install
npm run example-higher-order-components
```

This will bundle a runnable JS file, `example.js`, in the `examples/higher-order-components/dist` directory. You can view the running application by opening the corresponding `index.html` file.
