# Module Documentation

## Module Halogen.HTML.Widget


This module defines helper functions for working with third-party widgets.

#### `widget`

``` purescript
widget :: forall eff ctx val res. { destroy :: ctx -> HTMLElement -> Eff eff Unit, update :: val -> val -> ctx -> HTMLElement -> Eff eff (Maybe HTMLElement), init :: (res -> Eff eff Unit) -> Eff eff { node :: HTMLElement, context :: ctx }, id :: String, name :: String, value :: val } -> V.Widget eff res
```

Create a `VTree` from a third-party component (or _widget_), by providing a name, an ID, and three functions:

- An initialization function, which creates the DOM node, and receives a callback function which can
  be used to generate inputs
- An update function, which receives the previous DOM node and optionally creates a new one.
- A finalizer function, which deallocates any necessary resources when the component is removed from the DOM.

The three functions share a common piece of data of a hidden type `s`.



