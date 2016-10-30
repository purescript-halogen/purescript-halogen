module Component.Task where

import Prelude

import Data.Bifunctor (bimap)

import Control.Monad.State as CMS

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

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
task :: forall m. Task -> H.Component HH.HTML TaskQuery TaskMessage m
task initialState = H.component { render, eval, initialState }
  where

  render :: Task -> H.ComponentHTML TaskQuery
  render t =
    bimap id id $ HH.li_
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

  eval :: TaskQuery ~> H.ComponentDSL Task TaskQuery TaskMessage m
  eval (UpdateDescription desc next) = do
    CMS.modify (_ { description = desc })
    pure next
  eval (ToggleCompleted b next) = do
    CMS.modify (_ { completed = b })
    H.raise (Toggled b)
    pure next
  eval (Remove next) = do
    H.raise NotifyRemove
    pure next
  eval (IsCompleted continue) = do
    b <- CMS.gets (_.completed)
    pure (continue b)
