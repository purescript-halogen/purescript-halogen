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

ui :: forall eff p. Component State Input (Aff (AppEffects eff)) p
ui = ...

main :: Eff (AppEffects ()) Unit
main = launchAff $ do
  app <- runUI ui state
  ...
```


