## Module Data.Injector

#### `Prism`

``` purescript
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
```

Compatible with `Prism` from `purescript-lens`.

#### `Injector`

``` purescript
type Injector s a = Prism a a s s
```

Compatible with `PrismP` from `purescript-lens`.

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


