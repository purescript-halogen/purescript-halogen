# Halogen Examples

This folder contains a variety of examples demonstrating different Halogen features. You can compile these examples from the root of the project.

## Building & Running Examples

Each of the Halogen examples are self-contained in a directory containing the source code and an `index.html` file that you can open in your browser once the source is compiled.

First, make sure you have `purescript` installed (if you don't already):

```sh
npm install -g purescript
```

Then, make sure Halogen's dependencies are installed:

```sh
npm install
```

Now you can build any of the examples from the root of the repository using the command `npm run example-<name>`:

```text
npm run example-ace
npm run example-basic
# ...
```

This will compile the example source code into a file named `example.js` in the `dist` directory for the example. You can now open the corresponding `index.html` file from the same directory.
