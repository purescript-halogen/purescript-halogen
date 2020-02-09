## Halogen Examples

These example can be compiled from the root of the project. 

First, install `bower` and `purescript` globally, if you don't already have them:

```
npm install -g bower
npm install -g purescript
```

Then make sure Halogen's `npm` and `bower` dependencies are installed locally:

```
$ npm install
$ bower install
```

Then each project can be built using `npm run example-<name>`:

```text
$ npm run example-basic
```

If everything is successful the example will be built as `example.js` in the `dist` directory within the example, runnable by opening the corresponding `index.html`.
