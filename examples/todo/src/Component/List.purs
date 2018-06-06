module Component.List where

import Prelude

import Component.Task (TaskQuery(..), TaskMessage(..), TaskSlot, task)
import Data.Array (snoc, filter, length)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Model (List, TaskId, initialList, initialTask)

-- | The list component query algebra.
data ListQuery a
  = NewTask a
  | AllDone a
  | HandleTaskMessage TaskId TaskMessage a

type ChildSlots =
  ( task :: TaskSlot TaskId
  )

_task = SProxy :: SProxy "task"

-- | The list component definition.
list :: forall m. Applicative m => H.Component HH.HTML ListQuery Unit Void m
list =
  H.component
    { initialState: const initialList
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
  where

  render :: List -> H.ComponentHTML ListQuery ChildSlots m
  render st =
    HH.div_
      [ HH.h1_ [ HH.text "Todo list" ]
      , HH.p_
          [ HH.button
              [ HE.onClick (HE.input_ NewTask) ]
              [ HH.text "New Task" ]
          ]
      , HH.ul_ (map renderTask st.tasks)
      , HH.p_ [ HH.text $ show st.numCompleted <> " / " <> show (length st.tasks) <> " complete" ]
      , HH.button
          [ HE.onClick (HE.input_ AllDone) ]
          [ HH.text "All Done" ]
      ]

  renderTask :: TaskId -> H.ComponentHTML ListQuery ChildSlots m
  renderTask taskId =
    HH.slot _task taskId
      (task initialTask)
      unit
      (HE.input (HandleTaskMessage taskId))

  eval :: ListQuery ~> H.HalogenM List ListQuery ChildSlots Void m
  eval (NewTask next) = do
    H.modify_ addTask
    pure next
  eval (AllDone next) = do
    toggled <- H.queryAll _task (H.action (ToggleCompleted true))
    H.modify_ $ updateNumCompleted (const (M.size toggled))
    pure next
  eval (HandleTaskMessage p msg next) = do
    case msg of
      NotifyRemove -> do
        wasComplete <- H.query _task p (H.request IsCompleted)
        when (fromMaybe false wasComplete) $ H.modify_ $ updateNumCompleted (_ `sub` 1)
        H.modify_ (removeTask p)
      Toggled b ->
        H.modify_ $ updateNumCompleted (if b then (_ + 1) else (_ `sub` 1))
    pure next

-- | Adds a task to the current state.
addTask :: List -> List
addTask st = st { nextId = st.nextId + 1, tasks = st.tasks `snoc` st.nextId }

-- | Removes a task from the current state.
removeTask :: TaskId -> List -> List
removeTask taskId st = st { tasks = filter (_ /= taskId) st.tasks }

-- | Updates the number of completed tasks.
updateNumCompleted :: (Int -> Int) -> List -> List
updateNumCompleted f st = st { numCompleted = f st.numCompleted }
