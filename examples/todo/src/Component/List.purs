module Component.List where

import Prelude

import Data.Array (snoc, filter, length)
import Data.Functor.Coproduct (Coproduct)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH

import Model (List, Task, TaskId, initialTask)
import Component.Task (TaskQuery(..), task)

-- | The list component query algebra.
data ListQuery a
  = NewTask a
  | AllDone a

-- | The slot value that is filled by tasks during the install process.
newtype TaskSlot = TaskSlot TaskId
derive instance eqTaskSlot :: Eq TaskSlot
derive instance ordTaskSlot :: Ord TaskSlot

type State g = H.ParentState List Task ListQuery TaskQuery g TaskSlot
type Query = Coproduct ListQuery (H.ChildF TaskSlot TaskQuery)

-- | The list component definition.
list :: forall g. Functor g => H.Component (State g) Query g
list = H.parentComponent { render, eval, peek: Just peek }
  where

  render :: List -> H.ParentHTML Task ListQuery TaskQuery g TaskSlot
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

  renderTask :: TaskId -> H.ParentHTML Task ListQuery TaskQuery g TaskSlot
  renderTask taskId = HH.slot (TaskSlot taskId) \_ -> { component: task, initialState: initialTask }

  eval :: ListQuery ~> H.ParentDSL List Task ListQuery TaskQuery g TaskSlot
  eval (NewTask next) = do
    H.modify addTask
    pure next
  eval (AllDone next) = do
    toggled <- H.queryAll (H.action (ToggleCompleted true))
    H.modify $ updateNumCompleted (const (M.size toggled))
    pure next

  peek :: forall a. H.ChildF TaskSlot TaskQuery a -> H.ParentDSL List Task ListQuery TaskQuery g TaskSlot Unit
  peek (H.ChildF p q) = case q of
    Remove _ -> do
      wasComplete <- H.query p (H.request IsCompleted)
      when (fromMaybe false wasComplete) $ H.modify $ updateNumCompleted (_ `sub` 1)
      H.modify (removeTask p)
    ToggleCompleted b _ ->
      H.modify $ updateNumCompleted (if b then (_ + 1) else (_ `sub` 1))
    _ -> pure unit

-- | Adds a task to the current state.
addTask :: List -> List
addTask st = st { nextId = st.nextId + 1, tasks = st.tasks `snoc` st.nextId }

-- | Removes a task from the current state.
removeTask :: TaskSlot -> List -> List
removeTask (TaskSlot id) st = st { tasks = filter (_ /= id) st.tasks }

-- | Updates the number of completed tasks.
updateNumCompleted :: (Int -> Int) -> List -> List
updateNumCompleted f st = st { numCompleted = f st.numCompleted }
