module Component.Task where

import Prelude

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.HTML.Events.Indexed as HE

import Model (Task)

-- | The task component query algebra.
data TaskQuery a
  = UpdateDescription String a
  | ToggleCompleted Boolean a
  | Remove a
  | IsCompleted (Boolean -> a)

data TaskMessage
  = NotifyRemove
  | Toggled Boolean

-- | The task component definition.
task :: forall g. Task -> H.Component TaskQuery g TaskMessage
task initialState = H.component { render, eval, initialState }
  where

  render :: Task -> H.ComponentHTML TaskQuery g
  render t =
    HH.li_
      [ HH.input
          [ HP.inputType HP.InputCheckbox
          , HP.title "Mark as completed"
          , HP.checked t.completed
          , HE.onChecked (HE.input ToggleCompleted)
          ]
      , HH.input
          [ HP.inputType HP.InputText
          , HP.placeholder "Task description"
          , HP.autofocus true
          , HP.value t.description
          , HE.onValueChange (HE.input UpdateDescription)
          ]
      , HH.button
          [ HP.title "Remove task"
          , HE.onClick (HE.input_ Remove)
          ]
          [ HH.text "✖" ]
      ]

  eval :: TaskQuery ~> H.ComponentDSL Task TaskQuery g TaskMessage
  eval (UpdateDescription desc next) = do
    H.modify (_ { description = desc })
    pure next
  eval (ToggleCompleted b next) = do
    H.modify (_ { completed = b })
    H.raise (Toggled b)
    pure next
  eval (Remove next) = do
    H.raise NotifyRemove
    pure next
  eval (IsCompleted continue) = do
    b <- H.gets (_.completed)
    pure (continue b)
