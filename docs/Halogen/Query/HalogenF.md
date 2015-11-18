## Module Halogen.Query.HalogenF

#### `HalogenFP`

``` purescript
data HalogenFP (e :: (* -> *) -> (* -> *) -> *) s f g a
  = StateHF (StateF s a)
  | SubscribeHF (e f g) a
  | QueryHF (g a)
  | HaltHF
```

The Halogen component algebra

##### Instances
``` purescript
instance functorHalogenF :: (Functor g) => Functor (HalogenFP e s f g)
instance injectStateHF :: Inject (StateF s) (HalogenFP e s f g)
instance injectQueryHF :: Inject g (HalogenFP e s f g)
instance altHalogenF :: (Functor g) => Alt (HalogenFP e s f g)
instance plusHalogenF :: (Functor g) => Plus (HalogenFP e s f g)
```

#### `HalogenF`

``` purescript
type HalogenF = HalogenFP EventSource
```

#### `transformHF`

``` purescript
transformHF :: forall s s' f f' g g'. (Functor g, Functor g') => Natural (StateF s) (StateF s') -> Natural f f' -> Natural g g' -> Natural (HalogenF s f g) (HalogenF s' f' g')
```

Change all the parameters of `HalogenF`.

#### `hoistHalogenF`

``` purescript
hoistHalogenF :: forall s f g h. (Functor h) => Natural g h -> Natural (HalogenF s f g) (HalogenF s f h)
```

Changes the `g` for a `HalogenF`. Used internally by Halogen.


