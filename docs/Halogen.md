## Module Halogen

#### `Driver`

``` purescript
type Driver f eff = forall i. f i -> Aff (HalogenEffects eff) i
```

#### `runUI`

``` purescript
runUI :: forall eff s f. Component s f (Aff (HalogenEffects eff)) Void -> s -> Aff (HalogenEffects eff) { node :: HTMLElement, driver :: Driver f eff }
```

#### `actionF`

``` purescript
actionF :: forall f g. (Functor f, Functor g, Inject f g) => (forall i. i -> f i) -> Free g Unit
```

#### `requestF`

``` purescript
requestF :: forall f g a. (Functor f, Functor g, Inject f g) => (forall i. (a -> i) -> f i) -> Free g a
```

#### `actionFC`

``` purescript
actionFC :: forall f g. (Functor g, Inject (Coyoneda f) g) => (forall i. i -> f i) -> Free g Unit
```

#### `requestFC`

``` purescript
requestFC :: forall f g a. (Functor g, Inject (Coyoneda f) g) => (forall i. (a -> i) -> f i) -> Free g a
```


