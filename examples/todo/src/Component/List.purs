module Component.List where

import Prelude

import Control.Monad (when)

import Data.Array (snoc, filter, length)
import Data.Const (Const())
import Data.Functor (($>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Void (Void())

import Halogen
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E

import Model
import Component.Task

-- | The list component query algebra.
data ListInput a = NewTask a

-- | The list component definition.
list :: forall g p. (Functor g) => ParentComponentP State Task ListInput TaskInput g (ChildF TaskPlaceholder TaskInput) (Const Void) TaskPlaceholder p
list = component' render eval peek
  where

  render :: Render State ListInput TaskPlaceholder
  render st =
    H.div_ [ H.h1_ [ H.text "Todo list" ]
           , H.p_ [ H.button [ E.onClick (E.input_ NewTask) ]
                             [ H.text "New Task" ]
                  ]
           , H.ul_ (map (H.Placeholder <<< TaskPlaceholder) st.tasks)
           , H.p_ [ H.text $ show st.numCompleted ++ " / " ++ show (length st.tasks) ++ " complete" ]
           ]

  eval :: Eval ListInput State ListInput (QueryF State Task TaskInput g TaskPlaceholder p)
  eval (NewTask next) = modify addTask $> next

  peek :: Peek State ListInput (QueryF State Task TaskInput g TaskPlaceholder p) (ChildF TaskPlaceholder TaskInput)
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
removeTask :: TaskPlaceholder -> State -> State
removeTask (TaskPlaceholder id) st = st { tasks = filter (/= id) st.tasks }

-- | Updates the number of completed tasks.
updateNumCompleted :: (Int -> Int) -> State -> State
updateNumCompleted f st = st { numCompleted = f st.numCompleted }
