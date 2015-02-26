# Module Documentation

## Module Halogen

#### `Render`

``` purescript
type Render s i = s -> HTML i
```

A function which renders a component given an array of its rendered children and the current state

#### `FoldState`

``` purescript
type FoldState s i = s -> i -> s
```

A function which can respond to inputs by updating a state

#### `Spec`

``` purescript
newtype Spec s
```

A `Spec` defines a state machine which responds to inputs of some hidden type `i` and maintains a
state of type `s`.

#### `mkSpec`

``` purescript
mkSpec :: forall s i. Render s i -> FoldState s i -> Spec s
```

Create a `Spec` by providing a `render` function, and an operation
which updates the state given an input.

The type `i` is hidden in the return type.

#### `embed`

``` purescript
embed :: forall s1 s2. LensP s1 s2 -> Spec s2 -> Spec s1
```

`embed` allows us to enlarge the state types by using a `Lens`.

#### `beside`

``` purescript
beside :: forall s1 s2. Spec s1 -> Spec s2 -> Spec (Tuple s1 s2)
```

Side-by-side in a `div` element

#### `runSpec`

``` purescript
runSpec :: forall s eff. Spec s -> s -> Eff (dom :: DOM, ref :: Ref | eff) Node
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




