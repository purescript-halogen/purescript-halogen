module Component.List where

import Prelude

import Control.Plus (Plus)
import Control.Monad (when)

import Data.Array (snoc, filter, length)
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)
import Data.Maybe (fromMaybe)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Model
import Component.Task

-- | The list component query algebra.
data ListQuery a = NewTask a

-- | The slot value that is filled by tasks during the install process.
newtype TaskSlot = TaskSlot TaskId

derive instance genericTaskSlot :: Generic TaskSlot
instance eqTaskSlot :: Eq TaskSlot where eq = gEq
instance ordTaskSlot :: Ord TaskSlot where compare = gCompare

type State g = InstalledState List Task ListQuery TaskQuery g TaskSlot
type Query = Coproduct ListQuery (ChildF TaskSlot TaskQuery)

-- | The list component definition.
list :: forall g. (Functor g) => Component (State g) Query g
list = parentComponent' render eval peek
  where

  render :: List -> ParentHTML Task ListQuery TaskQuery g TaskSlot
  render st =
    H.div_ [ H.h1_ [ H.text "Todo list" ]
           , H.p_ [ H.button [ E.onClick (E.input_ NewTask) ]
                             [ H.text "New Task" ]
                  ]
           , H.ul_ (map renderTask st.tasks)
           , H.p_ [ H.text $ show st.numCompleted ++ " / " ++ show (length st.tasks) ++ " complete" ]
           ]

  renderTask :: TaskId -> ParentHTML Task ListQuery TaskQuery g TaskSlot
  renderTask taskId = H.slot (TaskSlot taskId) \_ -> { component: task, initialState: initialTask }

  eval :: Natural ListQuery (ParentDSL List Task ListQuery TaskQuery g TaskSlot)
  eval (NewTask next) = do
    modify addTask
    pure next

  peek :: forall a. ChildF TaskSlot TaskQuery a -> ParentDSL List Task ListQuery TaskQuery g TaskSlot Unit
  peek (ChildF p q) = case q of
    Remove _ -> do
      wasComplete <- query p (request IsCompleted)
      when (fromMaybe false wasComplete) $ modify $ updateNumCompleted (`sub` 1)
      modify (removeTask p)
    ToggleCompleted b _ -> modify $ updateNumCompleted (if b then (+ 1) else (`sub` 1))
    _ -> pure unit

-- | Adds a task to the current state.
addTask :: List -> List
addTask st = st { nextId = st.nextId + 1, tasks = st.tasks `snoc` st.nextId }

-- | Removes a task from the current state.
removeTask :: TaskSlot -> List -> List
removeTask (TaskSlot id) st = st { tasks = filter (/= id) st.tasks }

-- | Updates the number of completed tasks.
updateNumCompleted :: (Int -> Int) -> List -> List
updateNumCompleted f st = st { numCompleted = f st.numCompleted }
