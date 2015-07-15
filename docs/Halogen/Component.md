## Module Halogen.Component

#### `Component`

``` purescript
newtype Component s f g p
  = Component { render :: State s (HTML p (f Unit)), query :: forall i. f i -> StateT s g i }
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

#### `component`

``` purescript
component :: forall s f g p. (s -> HTML p (f Unit)) -> (forall i. f i -> StateT s g i) -> Component s f g p
```

#### `renderPure`

``` purescript
renderPure :: forall s p i. (s -> HTML p i) -> State s (HTML p i)
```

#### `todo`

``` purescript
todo :: forall a. a
```

#### `installR`

``` purescript
installR :: forall s f g pl pr s' f' p'. (Ord pr, Plus g) => Component s f (QueryT s s' f' pr p' g) (Either pl pr) -> (pr -> Tuple s' (Component s' f' g p')) -> Component (InstalledState s s' f' pl p' g) (Coproduct f (ChildF pr f')) g (Either pl p')
```

#### `installL`

``` purescript
installL :: forall s f g pl pr s' f' p'. (Ord pl, Plus g) => Component s f (QueryT s s' f' pl p' g) (Either pl pr) -> (pl -> Tuple s' (Component s' f' g p')) -> Component (InstalledState s s' f' pr p' g) (Coproduct f (ChildF pl f')) g (Either pr p')
```

#### `installAll`

``` purescript
installAll :: forall s f g p s' f' p'. (Ord p, Functor f, Functor f', Monad g, Plus g) => Component s f (QueryT s s' f' p p' g) p -> (p -> Tuple s' (Component s' f' g p')) -> Component (InstalledState s s' f' p p' g) (Coproduct f (ChildF p f')) g p'
```

#### `ChildF`

``` purescript
data ChildF p f i
  = ChildF p (f i)
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
  = QueryT (StateT (InstalledState s s' f' p p' g) g a)
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

#### `runQueryT`

``` purescript
runQueryT :: forall s s' f' p p' g a. QueryT s s' f' p p' g a -> StateT (InstalledState s s' f' p p' g) g a
```

#### `query`

``` purescript
query :: forall s s' f' p p' g. (Monad g, Ord p) => p -> (forall i. f' i -> QueryT s s' f' p p' g (Maybe i))
```


