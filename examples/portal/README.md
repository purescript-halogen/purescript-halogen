## Portal example

A simple component being rendered to a specific target element instead of its parent component. Note that the the tooltip breaks out of its container despite `overflow: hidden;` being set.

### Building

From the root of the Halogen project:

```
$ npm install
$ bower install
$ npm run example-portal
```

The code will be built as `example.js` in the `examples/portal/dist` directory within the example, runnable by opening the corresponding `index.html`.
