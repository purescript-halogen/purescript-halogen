module Example.Todo.Component.List where

import Prelude

import Data.Array (snoc, filter, length)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Example.Todo.Model (List, TaskId, initialList, initialTask)
import Example.Todo.Component.Task (TaskQuery(..), TaskMessage(..), TaskSlot, task)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

-- | The list component query algebra (queried from the outside).
type ListQuery a = Unit

-- | The list component actions (arising from the rendered HTML).
data ListAction
  = NewTask
  | AllDone
  | HandleTaskMessage TaskId TaskMessage

-- | The slots for child components.
type ListSlots = ( task :: TaskSlot TaskId )

-- | The slot entry for tasks.
_task :: SProxy "task"
_task = SProxy

-- | The list component definition.
list :: forall query input m. Applicative m => H.Component HH.HTML query input Void m
list =
  H.mkComponent
    { initialState: const initialList
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where

  -- Render the todo list component.
  render :: List -> H.ComponentHTML ListAction ListSlots m
  render l =
    HH.div_
      [ HH.h1_ [ HH.text "Todo List" ]
      , HH.p_
        [ HH.button
          [ HE.onClick $ const $ Just NewTask ]
          [ HH.text "New Task" ]
        ]
      , HH.ul_ (map renderTask l.tasks)
      , HH.p_ [ HH.text $ show l.numCompleted <> " / " <> show (length l.tasks) <> " complete" ]
      , HH.button
        [ HE.onClick $ const $ Just AllDone ]
        [ HH.text "All Done" ]
      ]

  -- Render an individual task, using the slot mechanism.
  renderTask :: TaskId -> H.ComponentHTML ListAction ListSlots m
  renderTask taskId =
    HH.slot
      _task
      taskId
      (task initialTask)
      unit
      (Just <<< (HandleTaskMessage taskId))

  -- Handle list actions. These actions arise from direct interaction with the rendered HTML.
  handleAction :: ListAction -> H.HalogenM List ListAction ListSlots Void m Unit
  handleAction = case _ of
    NewTask -> H.modify_ addTask
    AllDone -> do
      tasks <- H.queryAll _task $ H.tell $ SetCompleted true
      H.modify_ $ updateNumCompleted $ const $ M.size tasks
    HandleTaskMessage taskId taskMessage -> do
      case taskMessage of
        NotifyRemove -> do
          wasComplete <- H.query _task taskId $ H.request IsCompleted
          when (fromMaybe false wasComplete) $ H.modify_ $ updateNumCompleted (_ `sub` 1)
          H.modify_ (removeTask taskId)
        Toggled b -> H.modify_ $ updateNumCompleted (if b then (_ + 1) else (_ `sub` 1))

-- | Adds a task to the current state.
addTask :: List -> List
addTask st = st { nextId = st.nextId + 1, tasks = st.tasks `snoc` st.nextId }

-- | Removes a task from the current state.
removeTask :: TaskId -> List -> List
removeTask taskId st = st { tasks = filter (_ /= taskId) st.tasks }

-- | Updates the number of completed tasks.
updateNumCompleted :: (Int -> Int) -> List -> List
updateNumCompleted f st = st { numCompleted = f st.numCompleted }
