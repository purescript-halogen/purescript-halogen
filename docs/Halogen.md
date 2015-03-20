# Module Documentation

## Module Halogen


The main module of the Halogen library. It defines functions for running applications
assembled from the parts defined in the various submodules:

- `Halogen.Signal` for responding to inputs and maintaining state
- `Halogen.HTML.*` for templating HTML documents
- `Halogen.Themes.*` for rendering using common front-end libraries
- `Halogen.Mixin.*` for common additional application features

The type signature and documentation for the [`runUI`](#runUI) function provides a good introduction 
to this library. For more advanced use-cases, you might like to look at the `runUI` function instead.


#### `HalogenEffects`

``` purescript
type HalogenEffects eff = (dom :: DOM, ref :: Ref, trace :: Trace | eff)
```

Wraps the effects required by the `runUI` function.

#### `changes`

``` purescript
changes :: VTree -> SF VTree Patch
```

A signal which emits patches corresponding to successive `VTree`s.

This function can be used to create alternative top-level handlers which use `virtual-dom`.

#### `View`

``` purescript
type View i p r = SF1 i (H.HTML p (Either i r))
```

A view is represented as a pure, non-empty signal function which
consumes inputs of type `r`, and generates HTML documents.

The HTML documents can contain placeholders of type `p`, and
generate events which are either inputs (`i`) or requests (`r`). 

#### `PureView`

``` purescript
type PureView i = forall p. SF1 i (H.HTML p i)
```

A pure view does not make any external requests or use placeholder elements.

#### `Handler`

``` purescript
type Handler r i eff = r -> Aff (HalogenEffects eff) i
```

This type synonym is provided to tidy up the type signature of `runUI`.

The _handler function_ is responsible for receiving requests from the UI, integrating with external
components, and providing inputs back to the system based on the results.

For example:

```purescript
data Input = SetDateAndTime DateAndTime | ...

data Request = GetDateAndTimeRequest | ...

appHandler :: forall eff. Handler Request Input eff 
appHandler GetDateAndTimeRequest k =
  get "/date" \response -> k (readDateAndTime response)
```

#### `Driver`

``` purescript
type Driver i eff = i -> Eff (HalogenEffects eff) Unit
```

This type synonym is provided to tidy up the type signature of `runUI`.

The _driver function_ can be used by the caller to inject additional inputs into the system at the top-level.

This is useful for supporting applications which respond to external events which originate
outside the UI, such as timers or hash-change events.

For example, to drive the UI with a `Tick` input every second, we might write something like the following:

```purescript
main = do
  Tuple node driver <- runUI ui
  appendToBody node
  setInterval 1000 $ driver Tick
```

#### `Renderer`

``` purescript
type Renderer p = p -> VTree
```

A type synonym for functions which render components to replace placeholders

#### `UI`

``` purescript
type UI i p r eff = { renderer :: Renderer p, handler :: Handler r i eff, view :: View i p r }
```

A UI consists of:

- A view
- A handler function
- A function which renders placeholder elements

#### `PureUI`

``` purescript
type PureUI i = forall eff. UI i Void Void eff
```

A pure UI is a UI which:

- Does not render placeholder elements
- Does not make external requests

#### `pureUI`

``` purescript
pureUI :: forall i. (forall p. SF1 i (H.HTML p i)) -> PureUI i
```

A convenience function which can be used to construct a pure UI

#### `runUI`

``` purescript
runUI :: forall i p r eff. UI i p r eff -> Eff (HalogenEffects eff) (Tuple Node (Driver i eff))
```

`runUI` renders a `UI` to the DOM using `virtual-dom`.

This function is the workhorse of the Halogen library. It can be called in `main`
to set up the application and create the driver function, which can be used to 
send inputs to the UI from external components.



