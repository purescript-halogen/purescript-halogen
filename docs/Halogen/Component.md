## Module Halogen.Component

#### `Component`

``` purescript
newtype Component s f g p
```

##### Instances
``` purescript
instance functorComponent :: Functor (Component s f g)
```

#### `ComponentF`

``` purescript
type ComponentF s f = Component s (Free f)
```

#### `ComponentFC`

``` purescript
type ComponentFC s f = Component s (FreeC f)
```

#### `Render`

``` purescript
type Render s p f = s -> HTML p (f Unit)
```

#### `RenderF`

``` purescript
type RenderF s p f = s -> HTML p (Free f Unit)
```

#### `RenderFC`

``` purescript
type RenderFC s p f = s -> HTML p (FreeC f Unit)
```

#### `Eval`

``` purescript
type Eval f s g = Natural f (StateT s g)
```

#### `renderComponent`

``` purescript
renderComponent :: forall s f g p. Component s f g p -> s -> Tuple (HTML p (f Unit)) s
```

#### `queryComponent`

``` purescript
queryComponent :: forall s f g p i. Component s f g p -> f i -> s -> g (Tuple i s)
```

#### `component`

``` purescript
component :: forall s f g p. Render s p f -> Eval f s g -> Component s f g p
```

#### `componentF`

``` purescript
componentF :: forall s f g p. (MonadRec g, Functor f) => RenderF s p f -> Eval f s g -> ComponentF s f g p
```

#### `componentFC`

``` purescript
componentFC :: forall s f g p. (MonadRec g) => RenderFC s p f -> Eval f s g -> ComponentFC s f g p
```

#### `installL`

``` purescript
installL :: forall s f g pl pr s' f' p'. (Ord pl, Monad g, Plus g) => Component s f (QueryT s s' f' pl p' g) (Either pl pr) -> (pl -> ComponentState s' f' g p') -> Component (InstalledState s s' f' pl p' g) (Coproduct f (ChildF pl f')) g (Either p' pr)
```

#### `installR`

``` purescript
installR :: forall s f g pl pr s' f' p'. (Ord pr, Monad g, Plus g) => Component s f (QueryT s s' f' pr p' g) (Either pl pr) -> (pr -> ComponentState s' f' g p') -> Component (InstalledState s s' f' pr p' g) (Coproduct f (ChildF pr f')) g (Either pl p')
```

#### `installAll`

``` purescript
installAll :: forall s f g p s' f' p'. (Ord p, Monad g, Plus g) => Component s f (QueryT s s' f' p p' g) p -> (p -> ComponentState s' f' g p') -> Component (InstalledState s s' f' p p' g) (Coproduct f (ChildF p f')) g p'
```

#### `ChildF`

``` purescript
data ChildF p f i
```

##### Instances
``` purescript
instance functorChildF :: (Functor f) => Functor (ChildF p f)
```

#### `ComponentState`

``` purescript
type ComponentState s f g p = Tuple s (Component s f g p)
```

#### `InstalledState`

``` purescript
type InstalledState s s' f' p p' g = { parent :: s, children :: Map p (ComponentState s' f' g p') }
```

#### `QueryT`

``` purescript
newtype QueryT s s' f' p p' g a
```

##### Instances
``` purescript
instance functorQueryT :: (Monad g) => Functor (QueryT s s' f' p p' g)
instance applyQueryT :: (Monad g) => Apply (QueryT s s' f' p p' g)
instance applicativeQueryT :: (Monad g) => Applicative (QueryT s s' f' p p' g)
instance bindQueryT :: (Monad g) => Bind (QueryT s s' f' p p' g)
instance monadQueryT :: (Monad g) => Monad (QueryT s s' f' p p' g)
instance monadStateQueryT :: (Monad g) => MonadState s (QueryT s s' f' p p' g)
instance monadTransQueryT :: MonadTrans (QueryT s s' f' p p')
```

#### `query`

``` purescript
query :: forall s s' f' p p' g. (Monad g, Ord p) => p -> (forall i. f' i -> QueryT s s' f' p p' g (Maybe i))
```


