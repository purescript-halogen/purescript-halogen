## Higher order components example

This example illustrates the definition and use of higher order components. A
higher order component basically consists of a `factory`, which takes a
component (the "inner" component) and returns a new one.

In this example there is a container which has a button as a child, but the
button is wrapped using the higher order component.

The factory in this example illustrates the following properties:

* Inputs to the inner component get lifted
* Messages from the inner component get lifted
* The inner component can be queried using the `liftQuery` helper function
* The generated component can interact with inner components, as long as their
  query type has an instance of the `CanSet` typeclass.

### Building

From the root of the Halogen project:

```
$ npm install
$ npm run example-higher-order-components
```

Or from the current directory:

```
$ npm install
$ bower install
$ npm run build
```

The code will be built as `example.js` in the
`examples/higher-order-components/dist` directory within the example, runnable
by opening the corresponding `index.html`.
