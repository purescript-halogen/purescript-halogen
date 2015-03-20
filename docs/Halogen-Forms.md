# Module Documentation

## Module Halogen.HTML.Events.Forms


Convenience functions for working with form elements.

#### `onValueChanged`

``` purescript
onValueChanged :: forall attr value i. (H.AttrRepr attr, IsForeign value) => (value -> EventHandler i) -> attr i
```

Attach an event handler which will produce an input when the value of an input field changes

An input will not be produced if the value cannot be cast to the appropriate type.

#### `onChecked`

``` purescript
onChecked :: forall attr i. (H.AttrRepr attr) => (Boolean -> EventHandler i) -> attr i
```

Attach an event handler which will fire when a checkbox is checked or unchecked

#### `onInput`

``` purescript
onInput :: forall attr value i. (H.AttrRepr attr, IsForeign value) => (value -> EventHandler i) -> attr i
```

Attach an event handler which will fire on input



