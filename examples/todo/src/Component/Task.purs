module Example.Todo.Component.Task where

import Prelude

import Data.Maybe (Maybe(..))
import Example.Todo.Model (Task, initialTask)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | The task component query algebra.
data TaskQuery a
  = IsCompleted (Boolean -> a)
  | QueryAction TaskAction a

-- | The messages the task component might send to a parent component.
data TaskMessage
  = NotifyRemove
  | Toggled Boolean

-- | The task component actions (arising from the rendered HTML).
data TaskAction
  = UpdateDescription String
  | SetCompleted Boolean
  | Remove

-- | The task slot for use of this component in parent components.
type TaskSlot = H.Slot TaskQuery TaskMessage

-- | Child slots of the task component itself (none).
type ChildSlots = ()

-- | The task component definition
task :: forall m. Task -> H.Component HH.HTML TaskQuery Unit TaskMessage m
task initialState =
  H.mkComponent
    { initialState: const initialTask
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }
  where

  render :: Task -> H.ComponentHTML TaskAction ChildSlots m
  render t = HH.li_
    [ HH.input
      [ HP.type_ HP.InputCheckbox
      , HP.title "Mark as Completed"
      , HP.checked t.completed
      , HE.onChecked (Just <<< SetCompleted)
      ]
    , HH.input
      [ HP.type_ HP.InputText
      , HP.placeholder "Task description"
      , HP.autofocus true
      , HP.value t.description
      , HE.onValueChange (Just <<< UpdateDescription)
      ]
    , HH.button
      [ HP.title "Remove Task"
      , HE.onClick $ const $ Just Remove
      ]
      [ HH.text "✖" ]
    ]

handleAction :: forall m. TaskAction -> H.HalogenM Task TaskAction ChildSlots TaskMessage m Unit
handleAction = case _ of
  UpdateDescription desc -> H.modify_ $ _ { description = desc }
  SetCompleted b -> do
    currentValue <- H.get
    if currentValue.completed == b
      then pure unit
      else do
        H.modify_ $ _ { completed = b }
        H.raise $ Toggled b
  Remove -> do
    H.raise NotifyRemove

handleQuery :: forall a m. TaskQuery a -> H.HalogenM Task TaskAction ChildSlots TaskMessage m (Maybe a)
handleQuery = case _ of
  IsCompleted f -> do
    t <- H.get
    pure $ Just $ f t.completed
  QueryAction a next -> do
    handleAction a
    pure $ Just next
