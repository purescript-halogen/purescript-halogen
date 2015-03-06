module Halogen.Mixin.UndoRedo 
  ( UndoRedoInput(..)
  , UndoRedoState()
  
  , canUndo
  , canRedo
  , getState
  , undoRedoState
  
  , withUndo
  ) where

import Data.Maybe
import Data.Tuple

data Stack a = Empty | Push a (Stack a)

pop :: forall a. Stack a -> Maybe (Tuple a (Stack a))
pop (Push a s) = Just (Tuple a s)
pop Empty = Nothing

depth :: forall a. Stack a -> Number
depth Empty = 0
depth (Push _ s) = 1 + depth s

null :: forall a. Stack a -> Boolean
null Empty = true
null _ = false

-- | Adds two new input types:
-- |
-- | - `Undo` - move to the previous state
-- | - `Redo` - move to the next state
data UndoRedoInput i = Undo | Redo | Input i

-- | Modifies the state type to include its _past_ and _future_.
data UndoRedoState s = UndoRedoState (Stack s) s (Stack s)

-- | `true` if the state supports the undo operation. 
canUndo :: forall s. UndoRedoState s -> Boolean
canUndo (UndoRedoState past _ _) = not (null past)

-- | `true` if the state supports the redo operation.
canRedo :: forall s. UndoRedoState s -> Boolean
canRedo (UndoRedoState _ _ future) = not (null future) 

-- | Get the state at the current time
getState :: forall s. UndoRedoState s -> s
getState (UndoRedoState _ s _) = s

-- | Create a state with no past and no future
undoRedoState :: forall s. s -> UndoRedoState s
undoRedoState s = UndoRedoState Empty s Empty

-- | Lift a step function to support the undo and redo operations.
-- |
-- | The view should use the `canUndo` and `canRedo` functions to determine whether or not
-- | to enable the corresponding controls.
withUndo :: forall s i. (s -> i -> s) -> UndoRedoState s -> UndoRedoInput i -> UndoRedoState s
withUndo _ st@(UndoRedoState past s future) Undo = fromMaybe st $ do
  Tuple prev rest <- pop past
  return $ UndoRedoState rest prev (Push s future)
withUndo _ st@(UndoRedoState past s future) Redo = fromMaybe st $ do
  Tuple next rest <- pop future
  return $ UndoRedoState (Push s past) next rest
withUndo f (UndoRedoState past s _) (Input i) = UndoRedoState (Push s past) (f s i) Empty
