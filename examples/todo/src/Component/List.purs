module Component.List where

import Prelude

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
newtype ListSlot = ListSlot TaskId

derive instance genericListSlot :: Generic ListSlot
instance eqListSlot :: Eq ListSlot where eq = gEq
instance ordListSlot :: Ord ListSlot where compare = gCompare

-- | The list component definition.
list :: forall g p. (Functor g) => ParentComponentP State Task ListInput TaskInput g ListSlot p
list = component' render eval peek
  where

  render :: Render State ListInput ListSlot
  render st =
    H.div_ [ H.h1_ [ H.text "Todo list" ]
           , H.p_ [ H.button [ E.onClick (E.input_ NewTask) ]
                             [ H.text "New Task" ]
                  ]
           , H.ul_ (map (H.slot <<< ListSlot) st.tasks)
           , H.p_ [ H.text $ show st.numCompleted ++ " / " ++ show (length st.tasks) ++ " complete" ]
           ]

  eval :: EvalP ListInput State Task ListInput TaskInput g ListSlot p
  eval (NewTask next) = do
    modify addTask
    pure next

  peek :: Peek (ChildF ListSlot TaskInput) State Task ListInput TaskInput g ListSlot p
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
removeTask :: ListSlot -> State -> State
removeTask (ListSlot id) st = st { tasks = filter (/= id) st.tasks }

-- | Updates the number of completed tasks.
updateNumCompleted :: (Int -> Int) -> State -> State
updateNumCompleted f st = st { numCompleted = f st.numCompleted }
