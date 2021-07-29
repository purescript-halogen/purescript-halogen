# Halogen

[![Latest release](http://img.shields.io/github/release/purescript-halogen/purescript-halogen.svg)](https://github.com/purescript-halogen/purescript-halogen/releases)
[![CI](https://github.com/purescript-halogen/purescript-halogen/workflows/CI/badge.svg?branch=master)](https://github.com/purescript-halogen/purescript-halogen/actions?query=workflow%3ACI+branch%3Amaster)

Halogen is a type-safe library for building user interfaces in PureScript.

- **Declarative**
  Write simple views for each state in your application, and Halogen will efficiently and intelligently update the right components and re-render your user interface.
- **Component Architecture**
  Write encapsulated components which manage their own state, and compose them together to build complex user interfaces. Or, use a single component to implement an Elm-like architecture.
- **Entirely PureScript**
  Halogen and its virtual DOM implementation are written in PureScript. Halogen's performance and bundle sizes are roughly equivalent to popular JavaScript UI libraries like React and Angular.

[Read the documentation](https://purescript-halogen.github.io/purescript-halogen) to learn how to use Halogen in your own projects.

## Installation

Install Halogen with [Spago](https://github.com/purescript/spago):

```sh
spago install halogen
```

Or [create a new Halogen app from a template](https://github.com/purescript-halogen/purescript-halogen-template).

## Documentation

You can find the Halogen documentation [on the documentation site](https://purescript-halogen.github.io/purescript-halogen) or [in the docs folder](https://github.com/purescript-halogen/purescript-halogen/tree/master/docs). Documentation is divided into several categories:

- [Halogen Guide](https://github.com/purescript-halogen/purescript-halogen/tree/master/docs/guide)
- [Concepts Reference](https://github.com/purescript-halogen/purescript-halogen/tree/master/docs/concepts-reference) (Coming soon)
- [API Reference on Pursuit](https://pursuit.purescript.org/packages/purescript-halogen)
- [Major Version Changelog](https://github.com/purescript-halogen/purescript-halogen/tree/master/docs/changelog)

We also recommend these community resources for learning how to use Halogen in your applications:

- [Real World Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld) by [Thomas Honeyman](https://github.com/thomashoneyman)

There are several ways to get help if you get stuck using Halogen:

- [Open an issue](https://github.com/purescript-halogen/purescript-halogen/issues) if you have encountered a bug or problem.
- [Start a thread on the PureScript Discourse](https://discourse.purescript.org/) or browse the `#halogen` tag if you have general questions about Halogen.
- Join the [PureScript Discord](https://discord.com/invite/sMqwYUbvz6) to chat about Halogen with other PureScript users.

## Examples

This repository contains [several self-contained examples](https://github.com/purescript-halogen/purescript-halogen/tree/master/examples), ranging from a basic button to controlling external components.

You may also want to review the [Real World Halogen](https://github.com/thomashoneyman/purescript-halogen-realworld/) example application, which demonstrates routing, state management, authentication, making requests, and other real world examples with commented explanations.

## Contributing

The main purpose of this repository is to continue evolving Halogen, making it faster and easier to use. Halogen is developed in the open on GitHub and we're grateful for community-contributed bugfixes and improvements.

You can take part in improving Halogen by opening or participating in issues, opening pull requests to add new features, documentation, or tests, and by helping other Halogen users on Discord and Discourse.

### License & Credits

Halogen is licensed under the [Apache License 2.0](https://github.com/purescript-halogen/purescript-halogen/blob/master/LICENSE). The Halogen logo was designed by [Matthew Park](https://www.matthewparkart.com/).
