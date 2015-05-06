# Module Documentation

## Module Halogen


The main module of the Halogen library. It defines functions for running applications
assembled from the parts defined in the various submodules:

- `Halogen.Signal` for responding to inputs and maintaining state
- `Halogen.HTML.*` for templating HTML documents
- `Halogen.Component` for building application components
- `Halogen.Themes.*` for rendering using common front-end libraries
- `Halogen.Mixin.*` for common additional application features

The functionality of this library is completely described by the type signature of the `runUI`
function, which renders a `Component` to the DOM. The other modules exist to make the construction
of `Component`s as simple as possible.


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

#### `runUI`

``` purescript
runUI :: forall req eff. Component (Event (HalogenEffects eff)) req req -> Eff (HalogenEffects eff) (Tuple HTMLElement (Driver req eff))
```

`runUI` renders a `Component` to the DOM using `virtual-dom`.

This function is the workhorse of the Halogen library. It can be called in `main`
to set up the application and create the driver function, which can be used to 
send inputs to the UI from external components.

#### `runUIWith`

``` purescript
runUIWith :: forall req eff. Component (Event (HalogenEffects eff)) req req -> (req -> HTMLElement -> Driver req eff -> Eff (HalogenEffects eff) Unit) -> Eff (HalogenEffects eff) (Tuple HTMLElement (Driver req eff))
```

A variant of `runUI` which supports a _post-render hook_. This allows applications
to support third-party components or other custom behaviors by modifying the DOM after
each update.

This is considered an advanced feature, and should only be used with an understanding of
the rendering pipeline.



