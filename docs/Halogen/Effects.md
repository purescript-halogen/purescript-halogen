## Module Halogen.Effects

#### `HalogenEffects`

``` purescript
type HalogenEffects eff = (avar :: AVAR, err :: EXCEPTION, dom :: DOM | eff)
```

A type alias for the basic row of effects that Halogen uses. This can be
combined with app-specific effects to help keep type signatures more
manageable:

```purescript
type AppEffects eff = HalogenEffects (ajax :: AJAX, console :: CONSOLE | eff)

ui :: forall eff. Component State Query (Aff (AppEffects eff))
ui = ...

main :: Eff (AppEffects ()) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI ui state
  ...
```


