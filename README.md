# purescript-halogen

[![Latest release](http://img.shields.io/github/release/slamdata/purescript-halogen.svg)](https://github.com/slamdata/purescript-halogen/releases)
[![Build status](https://travis-ci.org/slamdata/purescript-halogen.svg?branch=master)](https://travis-ci.org/slamdata/purescript-halogen)

A declarative, type-safe UI library for PureScript.

- [Getting Started](#)
- [Introduction](#)
	- [Query algebras](#)
	- [State](#)
	- [Component definitions](#)
	- [Rendering](#)
		- [Event listeners](#)
	- [Evaluating queries](#)
		- [Eval’s free monad](#)
		- [Non-state effects](#)
	- [The driver](#)
	- [Child components](#)
		- [Parent components](#)
		- [Slots](#)
		- [Querying children](#)
		- [Peeking](#)
		- [Multiple types of child component](#)
	- [Widgets (3rd party components)](#)
		- [A widget component](#)
		- [Initializers and finalizers](#)
		- [Initializing the widget](#)
		- [Subscriptions and event sources](#)
		- [Widget queries](#)

## Getting Started

- Read the [guide](docs/)
- Take a look at some of the examples:
  - [A basic button](examples/basic/)
  - [AJAX requests](examples/effects-aff-ajax/)
  - [Rudimentary routing](examples/driver-routing/)
  - [TODO list](examples/todo/)
  - [Ace editor](examples/ace/) (external component integration)
- Clone the [template project](https://github.com/slamdata/purescript-halogen-template) and give it a try!

## Installation

```
bower install purescript-halogen
```

## Documentation

- Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-halogen).
