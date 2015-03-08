module Test.Main where

import Data.Tuple
import Data.Maybe
import Data.Array (zipWith, length, modifyAt, deleteAt, (..), (!!))

import Debug.Trace

import Control.Monad.Eff

import DOM

import Halogen
import Halogen.Signal

import qualified Halogen.Mixin.UndoRedo as U

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Forms as A

import qualified Halogen.Themes.Bootstrap3 as B

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node

type Task = { description :: String, completed :: Boolean }

-- | The state of the application
data State = State [Task]

-- | Inputs to the state machine
data Input 
  = NewTask
  | UpdateDescription Number String
  | MarkCompleted Number Boolean
  | RemoveTask Number
  | Undo
  | Redo
  
instance inputSupportsUndoRedo :: U.SupportsUndoRedo Input where
  fromUndoRedo U.Undo = Undo
  fromUndoRedo U.Redo = Redo
  toUndoRedo Undo = Just U.Undo
  toUndoRedo Redo = Just U.Redo
  toUndoRedo _ = Nothing

-- | The UI is a state machine, consuming inputs, and generating HTML documents which in turn, generate new inputs
ui :: forall eff. SF1 Input (H.HTML Input)
ui = view <$> stateful (U.undoRedoState (State [])) (U.withUndoRedo update)
  where
  view :: U.UndoRedoState State -> H.HTML Input
  view st = 
    case U.getState st of
      State ts ->
        H.div (A.class_ B.container)
              [ H.h1 (A.id_ "header") [ H.text "todo list" ]
              , toolbar st
              , tasks ts
              ]
              
  toolbar :: forall st. U.UndoRedoState st -> H.HTML Input
  toolbar st = H.p (A.class_ B.btnGroup)
                   [ H.button ( A.classes [ B.btn, B.btnPrimary ]
                                <> A.onclick (const NewTask) )
                              [ H.text "New Task" ]
                   , H.button ( A.class_ B.btn
                                <> A.enabled (U.canUndo st)
                                <> A.onclick (const Undo) )
                              [ H.text "Undo" ]
                   , H.button ( A.class_ B.btn
                                <> A.enabled (U.canRedo st)
                                <> A.onclick (const Redo) )
                              [ H.text "Redo" ]
                   ]
           
  tasks :: [Task] -> H.HTML Input
  tasks ts = H.table (A.classes [ B.table, B.tableStriped ]) 
                     [ H.thead_ [ H.th_ [ H.text "Task" ]
                                , H.th_ [ H.text "Completed" ] 
                                , H.th_ []
                                ] 
                     , H.tbody_ (zipWith task ts (0 .. length ts))         
                     ]
                  
              
  task :: Task -> Number -> H.HTML Input
  task task index =
    H.tr_ [ H.td_ [ H.input ( A.classes [ B.formControl ]
                              <> A.placeholder "Description"
                              <> A.onValueChanged (UpdateDescription index)
                              <> A.value task.description )
                            [] ]
          , H.td_ [ H.input ( A.classes [ B.formControl, B.checkbox]
                              <> A.type_ "checkbox"
                              <> A.checked task.completed
                              <> A.title "Mark as completed"
                              <> A.onChecked (MarkCompleted index) )
                            [] ]
          , H.td_ [ H.button ( A.classes [ B.btn, B.btnDefault ]
                               <> A.title "Remove task"
                               <> A.onclick \_ -> RemoveTask index )
                             [ H.text "✖" ] ]
          ]

  update :: State -> Input -> State
  update (State ts) NewTask = State (ts ++ [{ description: "", completed: false }])
  update (State ts) (UpdateDescription i description) = State $ modifyAt i (_ { description = description }) ts
  update (State ts) (MarkCompleted i completed) = State $ modifyAt i (_ { completed = completed }) ts
  update (State ts) (RemoveTask i) = State $ deleteAt i 1 ts
  
main = do
  node <- runUI ui
  appendToBody node
