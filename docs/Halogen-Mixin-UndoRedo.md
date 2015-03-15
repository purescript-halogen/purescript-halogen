# Module Documentation

## Module Halogen.Mixin.UndoRedo


This module provides a generic undo/redo capability.

#### `UndoRedoInput`

``` purescript
data UndoRedoInput
  = Undo 
  | Redo 
```

Adds two new input types:

- `Undo` - move to the previous state
- `Redo` - move to the next state

#### `SupportsUndoRedo`

``` purescript
class SupportsUndoRedo input where
  fromUndoRedo :: UndoRedoInput -> input
  toUndoRedo :: input -> Maybe UndoRedoInput
```

This type class identifies those input types which support the Undo and Redo actions

#### `undo`

``` purescript
undo :: forall i. (SupportsUndoRedo i) => i
```

The undo action

#### `redo`

``` purescript
redo :: forall i. (SupportsUndoRedo i) => i
```

The redo action

#### `UndoRedoState`

``` purescript
data UndoRedoState s
```

Modifies the state type to include its _past_ and _future_.

#### `canUndo`

``` purescript
canUndo :: forall s. UndoRedoState s -> Boolean
```

`true` if the state supports the undo operation. 

#### `canRedo`

``` purescript
canRedo :: forall s. UndoRedoState s -> Boolean
```

`true` if the state supports the redo operation.

#### `getState`

``` purescript
getState :: forall s. UndoRedoState s -> s
```

Get the state at the current time

#### `undoRedoState`

``` purescript
undoRedoState :: forall s. s -> UndoRedoState s
```

Create a state with no past and no future

#### `withUndoRedo`

``` purescript
withUndoRedo :: forall s i. (SupportsUndoRedo i) => (s -> i -> s) -> UndoRedoState s -> i -> UndoRedoState s
```

Lift a step function to support the undo and redo operations.

The view should use the `canUndo` and `canRedo` functions to determine whether or not
to enable the corresponding controls.



