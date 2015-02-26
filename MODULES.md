# Module Documentation

## Module Halogen

#### `Spec`

``` purescript
newtype Spec s i
```

A `Spec` defines a state machine which responds to inputs of type `i` and maintains a
state of type `s`.

#### `render`

``` purescript
render :: forall s i. Spec s i -> s -> HTML i
```


#### `foldState`

``` purescript
foldState :: forall s i. Spec s i -> s -> i -> s
```


#### `mkSpec`

``` purescript
mkSpec :: forall s i. (s -> HTML i) -> (s -> i -> s) -> Spec s i
```

Create a `Spec` by providing a `render` function, and an operation
which updates the state given an input.

#### `embed`

``` purescript
embed :: forall s1 s2 i1 i2. LensP s1 s2 -> PrismP i1 i2 -> i1 -> Spec s2 i2 -> Spec s1 i1
```

#### `beside`

``` purescript
beside :: forall s1 s2 i1 i2. Spec s1 i1 -> Spec s2 i2 -> Spec (Tuple s1 s2) (Either i1 i2)
```

Side-by-side in a `div` element

#### `runSpec`

``` purescript
runSpec :: forall s i eff. Spec s i -> s -> Eff (dom :: DOM, ref :: Ref | eff) Node
```

`runSpec` is responsible for taking a `Spec` and hooking up its event
handlers to rerender the DOM. It maintains the state of the component
using a `RefVal`.


## Module Halogen.HTML

#### `MouseEvent`

``` purescript
data MouseEvent
```


#### `Attribute`

``` purescript
data Attribute i
  = OnClick (MouseEvent -> i)
```

#### `functorAttribute`

``` purescript
instance functorAttribute :: Functor Attribute
```


#### `HTML`

``` purescript
data HTML i
```


#### `functorHTML`

``` purescript
instance functorHTML :: Functor HTML
```


#### `renderHtml`

``` purescript
renderHtml :: forall i eff. (i -> Eff eff Unit) -> HTML i -> VTree
```


#### `text`

``` purescript
text :: forall i. String -> HTML i
```


#### `button`

``` purescript
button :: forall i. [Attribute i] -> [HTML i] -> HTML i
```

#### `button'`

``` purescript
button' :: forall i. [HTML i] -> HTML i
```


#### `div`

``` purescript
div :: forall i. [Attribute i] -> [HTML i] -> HTML i
```


#### `div'`

``` purescript
div' :: forall i. [HTML i] -> HTML i
```




