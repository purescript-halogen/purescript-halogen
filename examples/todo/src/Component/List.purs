module Component.List where

import Prelude

import Control.Plus (Plus)
import Control.Monad (when)

import Data.Array (snoc, filter, length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Generic (Generic, gEq, gCompare)

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E

import Model
import Component.Task

-- | The list component query algebra.
data ListInput a = NewTask a

-- | The slot value that is filled by tasks during the install process.
newtype TaskSlot = TaskSlot TaskId

derive instance genericTaskSlot :: Generic TaskSlot
instance eqTaskSlot :: Eq TaskSlot where eq = gEq
instance ordTaskSlot :: Ord TaskSlot where compare = gCompare

-- | The list component definition.
list :: forall g. (Plus g) => ParentComponent State Task ListInput TaskInput g TaskSlot
list = parentComponent' render eval peek
  where

  render :: RenderParent State Task ListInput TaskInput g TaskSlot
  render st =
    H.div_ [ H.h1_ [ H.text "Todo list" ]
           , H.p_ [ H.button [ E.onClick (E.input_ NewTask) ]
                             [ H.text "New Task" ]
                  ]
           , H.ul_ (map (\tid -> H.slot (TaskSlot tid) task \_ -> initialTask) st.tasks)
           , H.p_ [ H.text $ show st.numCompleted ++ " / " ++ show (length st.tasks) ++ " complete" ]
           ]

  eval :: EvalParent ListInput State Task ListInput TaskInput g TaskSlot
  eval (NewTask next) = do
    modify addTask
    pure next

  peek :: Peek (ChildF TaskSlot TaskInput) State Task ListInput TaskInput g TaskSlot
  peek (ChildF p q) = case q of
    Remove _ -> do
      wasComplete <- query p (request IsCompleted)
      when (fromMaybe false wasComplete) $ modify $ updateNumCompleted (`sub` 1)
      modify (removeTask p)
    ToggleCompleted b _ -> modify $ updateNumCompleted (if b then (+ 1) else (`sub` 1))
    _ -> pure unit

-- | Adds a task to the current state.
addTask :: State -> State
addTask st = st { nextId = st.nextId + 1, tasks = st.tasks `snoc` st.nextId }

-- | Removes a task from the current state.
removeTask :: TaskSlot -> State -> State
removeTask (TaskSlot id) st = st { tasks = filter (/= id) st.tasks }

-- | Updates the number of completed tasks.
updateNumCompleted :: (Int -> Int) -> State -> State
updateNumCompleted f st = st { numCompleted = f st.numCompleted }
