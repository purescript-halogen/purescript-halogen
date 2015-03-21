# Module Documentation

## Module Halogen.HTML.Events.Forms


Convenience functions for working with form elements.

#### `onValueChanged`

``` purescript
onValueChanged :: forall value i. (IsForeign value) => (value -> EventHandler i) -> H.Attr i
```

Attach an event handler which will produce an input when the value of an input field changes

An input will not be produced if the value cannot be cast to the appropriate type.

#### `onChecked`

``` purescript
onChecked :: forall i. (Boolean -> EventHandler i) -> H.Attr i
```

Attach an event handler which will fire when a checkbox is checked or unchecked

#### `onInput`

``` purescript
onInput :: forall value i. (IsForeign value) => (value -> EventHandler i) -> H.Attr i
```

Attach an event handler which will fire on input



