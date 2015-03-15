# Module Documentation

## Module Halogen.Mixin.Aff


Helper functions for working with the `Aff` monad.

#### `SupportsErrors`

``` purescript
class SupportsErrors input where
  liftError :: Error -> input
```

This type class identifies those input types which support errors

#### `HandlerAff`

``` purescript
type HandlerAff r i eff = r -> Aff (HalogenEffects eff) i
```

This type synonym is provided to tidy up the signature of `runUIAff`.

#### `runUIAff`

``` purescript
runUIAff :: forall i a r eff. (SupportsErrors i) => SF1 i (HTML a (Either i r)) -> (a -> VTree) -> HandlerAff r i eff -> EffA (HalogenEffects eff) (Tuple Node (Driver i eff))
```

A convenience function which uses the `Aff` monad to represent the handler function.



