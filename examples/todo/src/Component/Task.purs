module Component.Task where

import Prelude

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

import Model

-- | The task component query algebra.
data TaskInput a
  = UpdateDescription String a
  | ToggleCompleted Boolean a
  | Remove a
  | IsCompleted (Boolean -> a)

-- | The task component definition.
task :: forall g. (Functor g) => Component Task TaskInput g
task = component render eval
  where

  render :: Render Task TaskInput
  render t =
    H.li_ [ H.input [ P.inputType P.InputCheckbox
                    , P.title "Mark as completed"
                    , P.checked t.completed
                    , E.onChecked (E.input ToggleCompleted)
                    ]
          , H.input [ P.inputType P.InputText
                    , P.placeholder "Task description"
                    , P.value t.description
                    , E.onValueChange (E.input UpdateDescription)
                    ]
          , H.button [ P.title "Remove task"
                     , E.onClick (E.input_ Remove)
                     ]
                     [ H.text "✖" ]
          ]

  eval :: Eval TaskInput Task TaskInput g
  eval (UpdateDescription desc next) = do
    modify (_ { description = desc })
    pure next
  eval (ToggleCompleted b next) = do
    modify (_ { completed = b })
    pure next
  eval (Remove next) = pure next
  eval (IsCompleted continue) = do
    b <- gets (_.completed)
    pure (continue b)
