module Component.Task where

import Prelude

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
