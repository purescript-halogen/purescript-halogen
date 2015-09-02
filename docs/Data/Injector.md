## Module Data.Injector

#### `Injector`

``` purescript
data Injector a b
  = Injector (a -> b) (b -> Maybe a)
```

##### Instances
``` purescript
instance semigroupoidInjector :: Semigroupoid Injector
instance categoryInjector :: Category Injector
```

#### `inj`

``` purescript
inj :: forall a b. Injector a b -> a -> b
```

#### `prj`

``` purescript
prj :: forall a b. Injector a b -> b -> Maybe a
```

#### `injLE`

``` purescript
injLE :: forall a b. Injector a (Either a b)
```

#### `injRE`

``` purescript
injRE :: forall a b. Injector a (Either b a)
```

#### `injLC`

``` purescript
injLC :: forall f g a. Injector (f a) (Coproduct f g a)
```

#### `injRC`

``` purescript
injRC :: forall f g a. Injector (f a) (Coproduct g f a)
```


