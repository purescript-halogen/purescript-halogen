module Example.HOC.Harness (component) where

import Prelude

import Data.Maybe (Maybe(..))
import Example.HOC.Button as Button
import Example.HOC.Panel as Panel
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

data Action
  = CheckButtonState
  | HandlePanelMessage (Panel.Message Button.Message)

type State =
  { buttonCheckState :: Maybe Boolean
  , buttonMessageState :: Maybe Boolean
  }

type ChildSlots =
  ( panel :: Panel.Slot Button.Query Button.Message Unit
  )

_panel = Proxy :: Proxy "panel"

component :: forall q i o m. H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState ∷ forall i. i -> State
initialState _ = { buttonCheckState: Nothing, buttonMessageState: Nothing }

render :: forall m. State -> H.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ HH.slot _panel unit panelComponent unit HandlePanelMessage
    , HH.div_
        [ HH.button
            [ HE.onClick \_ -> CheckButtonState ]
            [ HH.text "Check button state" ]
        , HH.p_
            [ HH.text ("Last result: " <> printButtonState state.buttonCheckState) ]
        ]
    , HH.div_
        [ HH.p_
            [ HH.text ("Last message from the button: " <> printButtonState state.buttonMessageState ) ]]
    ]

printButtonState :: Maybe Boolean -> String
printButtonState = case _ of
  Nothing -> "Unknown"
  Just b -> if b then "On" else "Off"

panelComponent :: forall m. H.Component (Panel.Query Button.Query) Unit (Panel.Message Button.Message) m
panelComponent = Panel.component Button.component

handleAction :: forall o m. Action -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  CheckButtonState -> do
    buttonCheckState <- H.request _panel unit (Panel.QueryInner <<< Button.IsOn)
    H.modify_ (_ { buttonCheckState = buttonCheckState })
  HandlePanelMessage msg ->
    handlePanelMessage msg

handlePanelMessage :: forall o m. Panel.Message Button.Message -> H.HalogenM State Action ChildSlots o m Unit
handlePanelMessage = case _ of
  Panel.Opened ->
    pure unit
  Panel.Closed ->
    H.modify_ (_ { buttonMessageState = Nothing })
  Panel.Bubble msg ->
    handleButtonMessage msg

handleButtonMessage  :: forall o m. Button.Message -> H.HalogenM State Action ChildSlots o m Unit
handleButtonMessage = case _ of
  Button.Toggled b ->
    H.modify_ (_ { buttonMessageState = Just b })
