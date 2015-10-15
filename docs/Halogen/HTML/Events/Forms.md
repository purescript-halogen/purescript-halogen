## Module Halogen.HTML.Events.Forms

Convenience functions for working with form events.

#### `onValueChange`

``` purescript
onValueChange :: forall f. (String -> EventHandler (f Unit)) -> Prop (f Unit)
```

Attaches an event handler which will produce an input when the value of an
input field changes.

#### `onValueInput`

``` purescript
onValueInput :: forall f. (String -> EventHandler (f Unit)) -> Prop (f Unit)
```

Attaches an event handler which will fire on input.

#### `onChecked`

``` purescript
onChecked :: forall f. (Boolean -> EventHandler (f Unit)) -> Prop (f Unit)
```

Attaches an event handler which will fire when a checkbox is checked or
unchecked.


