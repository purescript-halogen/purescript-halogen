module Main where

import Data.Void
import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Array (zipWith, length, modifyAt, deleteAt, (..), (!!))

import qualified Data.String as S

import Debug.Trace

import Control.Functor (($>))
import Control.Alternative
import Control.Bind
import Control.Monad.Eff

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.Mixin.UndoRedo as Undo
import qualified Halogen.Mixin.Router as Router

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.HTML.CSS as CSS

import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.Themes.Bootstrap3.InputGroup as BI

import Css.Size (em)
import Css.Font (fontSize)

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

newtype Task = Task { description :: String, completed :: Boolean }

-- | The state of the application
data State = State [Task]

-- | Inputs to the state machine
data Input
  = NewTask (Maybe String)
  | UpdateDescription Number String
  | MarkCompleted Number Boolean
  | RemoveTask Number
  | Undo
  | Redo

instance inputSupportsUndoRedo :: Undo.SupportsUndoRedo Input where
  fromUndoRedo Undo.Undo = Undo
  fromUndoRedo Undo.Redo = Redo
  toUndoRedo Undo = Just Undo.Undo
  toUndoRedo Redo = Just Undo.Redo
  toUndoRedo _ = Nothing

-- | The view is a state machine, consuming inputs, and generating HTML documents which in turn, generate new inputs
ui :: forall m. (Alternative m) => Component m Input Input
ui = render <$> stateful (Undo.undoRedoState (State [])) (Undo.withUndoRedo update)
  where
  render :: Undo.UndoRedoState State -> H.HTML (m Input)
  render st =
    case Undo.getState st of
      State ts ->
        H.div [ A.class_ B.container ]
              [ H.h1 [ CSS.style (fontSize (em 3.0)) ] [ H.text "todo list" ]
              , toolbar st
              , H.div_ (zipWith task ts (0 .. length ts))
              ]

  toolbar :: forall st. Undo.UndoRedoState st -> H.HTML (m Input)
  toolbar st = H.p [ A.class_ B.btnGroup ]
                   [ H.button [ A.classes [ B.btn, B.btnPrimary ]
                              , A.onClick (A.input_ $ NewTask Nothing)
                              ]
                              [ H.text "New Task" ]
                   , H.button [ A.class_ B.btn
                              , A.enabled (Undo.canUndo st)
                              , A.onClick (A.input_ Undo)
                              ]
                              [ H.text "Undo" ]
                   , H.button [ A.class_ B.btn
                              , A.enabled (Undo.canRedo st)
                              , A.onClick (A.input_ Redo)
                              ]
                              [ H.text "Redo" ]
                   ]

  task :: Task -> Number -> H.HTML (m Input)
  task (Task task) index = H.p_ <<< pure $
    BI.inputGroup
      (Just (BI.RegularAddOn
        (H.input [ A.class_ B.checkbox
                 , A.type_ "checkbox"
                 , A.checked task.completed
                 , A.title "Mark as completed"
                 , A.onChecked (A.input (MarkCompleted index))
                 ]
                 [])))
      (H.input [ A.classes [ B.formControl ]
               , A.placeholder "Description"
               , A.onValueChanged (A.input (UpdateDescription index))
               , A.value task.description
               ]
               [])
      (Just (BI.ButtonAddOn
        (H.button [ A.classes [ B.btn, B.btnDefault ]
                  , A.title "Remove task"
                  , A.onClick (A.input_ $ RemoveTask index)
                  ]
                  [ H.text "✖" ])))

  update :: State -> Input -> State
  update (State ts) (NewTask s) = State (ts ++ [Task { description: fromMaybe "" s, completed: false }])
  update (State ts) (UpdateDescription i description) = State $ modifyAt i (\(Task t) -> Task (t { description = description })) ts
  update (State ts) (MarkCompleted i completed) = State $ modifyAt i (\(Task t) -> Task (t { completed = completed })) ts
  update (State ts) (RemoveTask i) = State $ deleteAt i 1 ts

main = do
  Tuple node driver <- runUI ui
  appendToBody node
  Router.onHashChange (NewTask <<< Just <<< S.drop 1 <<< Router.runHash) driver
