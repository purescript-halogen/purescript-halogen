# Module Documentation

## Module Halogen.Mixin.ContT


Helper functions for working with the `ContT` monad.

#### `HandlerCont`

``` purescript
type HandlerCont r i eff = r -> ContT Unit (Eff (HalogenEffects eff)) i
```

This type synonym is provided to tidy up the signature of `runUICont`.

#### `runUICont`

``` purescript
runUICont :: forall i a r eff. SF1 i (HTML a (Either i r)) -> (a -> VTree) -> HandlerCont r i eff -> Eff (HalogenEffects eff) (Tuple Node (Driver i eff))
```

A convenience function which uses the `ContT` monad to represent the handler function.



