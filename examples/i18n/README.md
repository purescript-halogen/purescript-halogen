## I18n example

This example illustrates the use of `purescript-reflection` to propagate a
translation dictionary implicitly throughout the application. The dictionary
is carried through the `Given` typeclass. Components reify this dicionary and
use it in the render function.

In this example two instances of the same application are run, but each
is given a different translation dictionary.

### Building

From the root of the Halogen project:

```
$ npm install
$ npm run example-i18n
```

Or from the current directory:

```
$ npm install
$ bower install
$ npm run build
```

The code will be built as `example.js` in the
`examples/i18n/dist` directory within the example, runnable
by opening the corresponding `index.html`.
