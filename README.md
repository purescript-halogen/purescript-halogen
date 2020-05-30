# Halogen [![Latest release](http://img.shields.io/github/release/purescript-halogen/purescript-halogen.svg)](https://github.com/purescript-halogen/purescript-halogen/releases) [![halogen in package-sets](https://img.shields.io/endpoint.svg?url=https://package-sets-badge-0lf69kxs4fbd.runkit.sh/halogen)](https://github.com/purescript/package-sets) [![Build status](https://travis-ci.com/purescript-halogen/purescript-halogen.svg?branch=master)](https://travis-ci.com/purescript-halogen/purescript-halogen)

Halogen is a type-safe library for building user interfaces in PureScript.

* **Declarative**
  Write simple views for each state in your application, and Halogen will efficiently and intelligently update the right components and re-render your user interface.
* **Component Architecture**
  Write encapsulated components which manage their own state, and compose them together to build complex user interfaces. Or, use a single component to implement an Elm-like architecture.
* **Entirely PureScript**
  Halogen and its virtual DOM implementation are written in PureScript. Halogen's performance and bundle sizes are roughly equivalent to popular JavaScript UI libraries like React and Angular.

[Read the guide](https://github.com/purescript-halogen/purescript-halogen/tree/master/docs) to learn how to use Halogen in your own projects.

## Installation

Install Halogen with [Spago](https://github.com/purescript/spago):

```sh
spago install halogen
```

Or [create a new Halogen app from a template](https://github.com/purescript-halogen/purescript-halogen-template).

## Documentation

You can find the Halogen documentation [in the docs folder](https://github.com/purescript-halogen/purescript-halogen/tree/master/docs). Documentation is divided into several categories:

* [Halogen Guide](https://github.com/purescript-halogen/purescript-halogen/tree/master/docs/guide)
* [Concepts Reference](https://github.com/purescript-halogen/purescript-halogen/tree/master/docs/concepts-reference) (Coming soon)
* [API Reference on Pursuit](https://pursuit.purescript.org/packages/purescript-halogen)
* [Major Version Changelog](https://github.com/purescript-halogen/purescript-halogen/tree/master/docs/changelog)

We also recommend these community resources for learning how to use Halogen in your applications:

* [Learn Halogen](https://github.com/JordanMartinez/learn-halogen) by [Jordan Martinez](https://github.com/JordanMartinez)
* [Real World Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld) by [Thomas Honeyman](https://github.com/thomashoneyman)

There are several ways to get help if you get stuck using Halogen:

* [Open an issue](https://github.com/purescript-halogen/purescript-halogen/issues) if you have encountered a bug or problem.
* [Start a thread on the PureScript Discourse](https://discourse.purescript.org/) or browse the `#halogen` tag if you have general questions about Halogen.
* Join the `#purescript` and `#purescript-beginners` channels on the [Functional Programming Slack](https://functionalprogramming.slack.com) ([invite link](https://fpchat-invite.herokuapp.com/)) to chat about Halogen with other PureScript users.

## Examples

This repository contains [several self-contained examples](https://github.com/purescript-halogen/purescript-halogen/tree/master/examples), ranging from a basic button to controlling external components.

You may also want to review the [Real World Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld/) example application, which demonstrates routing, state management, authentication, making requests, and other real world examples with commented explanations.

## Contributing

The main purpose of this repository is to continue evolving Halogen, making it faster and easier to use. Halogen is developed in the open on GitHub and we're grateful for community-contributed bugfixes and improvements.

You can take part in improving Halogen by opening or participating in issues, opening pull requests to add new features, documentation, or tests, and by helping other Halogen users on Slack and Discourse.

### License & Credits

Halogen is licensed under the [Apache License 2.0](https://github.com/purescript-halogen/purescript-halogen/blob/master/LICENSE). The Halogen logo was designed by [Matthew Park](https://www.matthewparkart.com/).
