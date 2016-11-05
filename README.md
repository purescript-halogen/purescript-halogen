# purescript-halogen

[![Latest release](http://img.shields.io/github/release/slamdata/purescript-halogen.svg)](https://github.com/slamdata/purescript-halogen/releases)
[![Build status](https://travis-ci.org/slamdata/purescript-halogen.svg?branch=master)](https://travis-ci.org/slamdata/purescript-halogen)

A declarative, type-safe UI library for PureScript.

## Getting Started

- Read the [guide](GUIDE.md)
- Take a look at some of the examples:
  - [A basic counter](examples/counter/)
  - [AJAX requests](examples/ajax/)
  - [TODO list](examples/todo/)
  - [Ace editor](examples/ace/)
- Clone the [template project](https://github.com/slamdata/purescript-halogen-template) and give it a try!

## Installation

```
bower install purescript-halogen
```

`purescript-halogen` uses the `virtual-dom` library as a CommonJS dependency. To set up `virtual-dom` in your project, it is recommended that you:

- Install `virtual-dom` as an NPM dependency in your `package.json` file.
- Use `psc` with `psc-bundle` and then `webpack` or `browserify` to build and link the `virtual-dom` source code into a JS bundle for use in the web browser.
- If you're building with `pulp` then this is easy to do with the `pulp browserify` command.

## Documentation

- [The guide](GUIDE.md) attempts to cover all the common uses cases and features of Halogen.
- Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-halogen).
