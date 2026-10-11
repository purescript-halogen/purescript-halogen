# Changelog

## Unreleased

## 7.1.0 - 2026-10-11

### New features

- Add pointer-event handlers, including pointer capture events ([#794](https://github.com/purescript-halogen/purescript-halogen/pull/794)).
- Re-export `tellAll` from `Halogen` ([#796](https://github.com/purescript-halogen/purescript-halogen/pull/796)).
- Add the `aria-current` property via `Halogen.HTML.Properties.ARIA.current` ([#813](https://github.com/purescript-halogen/purescript-halogen/pull/813)).
- Add `mapState` for non-isomorphic state mapping, `mapHalogen` for mapping state, action, output, and monad together, and `identityLens` ([#817](https://github.com/purescript-halogen/purescript-halogen/pull/817)).
- Add the `onBeforeInput` event handler ([#819](https://github.com/purescript-halogen/purescript-halogen/pull/819)).

### Bug fixes

- Read the current component state from `selfRef` during finalization so cleanup uses the latest subscriptions and forks ([#827](https://github.com/purescript-halogen/purescript-halogen/pull/827)).

### Tooling

- Migrate the library, tests, and examples to registry-backed Spago, preserving published dependency bounds and declaring the dependency used by the new pointer-event handlers.
- Update repository tooling and CI actions.

Changelogs can be found in the [docs/changelog](https://github.com/purescript-halogen/purescript-halogen/tree/master/docs/changelog) directory.
