## Module Halogen.Component.Inject

#### `Inject`

``` purescript
data Inject a b
  = Inject (a -> b) (b -> Maybe a)
```

##### Instances
``` purescript
instance semigroupoidInject :: Semigroupoid Inject
instance categoryInject :: Category Inject
```

#### `inj`

``` purescript
inj :: forall a b. Inject a b -> a -> b
```

#### `prj`

``` purescript
prj :: forall a b. Inject a b -> b -> Maybe a
```

#### `injLE`

``` purescript
injLE :: forall a b. Inject a (Either a b)
```

#### `injRE`

``` purescript
injRE :: forall a b. Inject a (Either b a)
```

#### `injLC`

``` purescript
injLC :: forall f g a. Inject (f a) (Coproduct f g a)
```

#### `injRC`

``` purescript
injRC :: forall f g a. Inject (f a) (Coproduct g f a)
```

#### `InjectC`

``` purescript
data InjectC s s' f f' p p'
  = InjectC (Inject s s') (forall a. Inject (f a) (f' a)) (Inject p p')
```

#### `injState`

``` purescript
injState :: forall s s' f f' p p'. InjectC s s' f f' p p' -> s -> s'
```

#### `prjState`

``` purescript
prjState :: forall s s' f f' p p'. InjectC s s' f f' p p' -> s' -> Maybe s
```

#### `injQuery`

``` purescript
injQuery :: forall s s' f f' p p'. InjectC s s' f f' p p' -> Natural f f'
```

#### `prjQuery`

``` purescript
prjQuery :: forall s s' f f' p p' a. InjectC s s' f f' p p' -> f' a -> Maybe (f a)
```

#### `injSlot`

``` purescript
injSlot :: forall s s' f f' p p'. InjectC s s' f f' p p' -> p -> p'
```

#### `prjSlot`

``` purescript
prjSlot :: forall s s' f f' p p'. InjectC s s' f f' p p' -> p' -> Maybe p
```

#### `composeInjC`

``` purescript
composeInjC :: forall s t u f g h p q r. InjectC t u g h q r -> InjectC s t f g p q -> InjectC s u f h p r
```

#### `(:>)`

``` purescript
(:>) :: forall s t u f g h p q r. InjectC t u g h q r -> InjectC s t f g p q -> InjectC s u f h p r
```

_left-associative / precedence -1_

#### `inl`

``` purescript
inl :: forall s t f g p q. InjectC s (Either s t) f (Coproduct f g) p (Either p q)
```

#### `inr`

``` purescript
inr :: forall s t f g p q. InjectC s (Either t s) f (Coproduct g f) p (Either q p)
```


