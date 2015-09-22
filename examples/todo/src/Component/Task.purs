module Component.Task where

import Prelude

import Data.Functor (($>))
import Data.Tuple (Tuple(..))

import Halogen
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as P
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML.Events.Forms as E

import Model

-- | The task component query algebra.
data TaskInput a
  = UpdateDescription String a
  | ToggleCompleted Boolean a
  | Remove a
  | IsCompleted (Boolean -> a)

-- | The slot used when installing task components into a parent component.
newtype TaskSlot = TaskSlot TaskId

instance eqTaskSlot :: Eq TaskSlot where
  eq (TaskSlot x) (TaskSlot y) = x == y

instance ordTaskSlot :: Ord TaskSlot where
  compare (TaskSlot x) (TaskSlot y) = compare x y

-- | Creates a `ComponentState` entry based on a `TaskSlot`, used to
-- | install task components into a parent component.
mkTask :: forall g p. (Functor g) => TaskSlot -> ComponentState Task TaskInput g p
mkTask (TaskSlot _) = Tuple task { description: "", completed: false }

-- | The task component definition.
task :: forall g p. (Functor g) => Component Task TaskInput g p
task = component render eval
  where

  render :: Render Task TaskInput p
  render t =
    H.li_ [ H.input [ P.type_ "checkbox"
                    , P.title "Mark as completed"
                    , P.checked t.completed
                    , E.onChecked (E.input ToggleCompleted)
                    ]
          , H.input [ P.type_ "text"
                    , P.slot "Task description"
                    , P.value t.description
                    , E.onValueChange (E.input UpdateDescription)
                    ]
          , H.button [ P.title "Remove task"
                     , E.onClick (E.input_ Remove)
                     ]
                     [ H.text "✖" ]
          ]

  eval :: Eval TaskInput Task TaskInput g
  eval (UpdateDescription desc next) = modify (_ { description = desc }) $> next
  eval (ToggleCompleted b next) = modify (_ { completed = b }) $> next
  eval (Remove next) = pure next
  eval (IsCompleted continue) = gets (_.completed) >>= pure <<< continue
