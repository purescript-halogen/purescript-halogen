module Log where

import Prelude
import Data.Array as A
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State =
  { messages :: Array String
  , inputText :: String
  }

data Query a
  = AddMessage String a
  | UpdateInputText String a
  | SendMessage a

data Message
  = OutputMessage String                 

component :: forall m. H.Component HH.HTML Query Unit Message m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { messages: [] , inputText: "" }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.messages
      , HH.input
          [ HP.type_ HP.InputText
          , HP.value (state.inputText)
          , HE.onValueInput (HE.input UpdateInputText)
          ]
      , HH.button
          [ HE.onClick (HE.input_ SendMessage) ]
          [ HH.text "Send Message" ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (AddMessage msg next) = do
    let incomingMessage = "Received: " <> msg
    H.modify \st -> st { messages = st.messages `A.snoc` incomingMessage }
    pure next
  eval (SendMessage next) = do
    st <- H.get
    let outgoingMessage = st.inputText
    H.raise $ OutputMessage outgoingMessage
    H.modify \st' -> st'
      { messages = st'.messages `A.snoc` ("Sending: " <> outgoingMessage)
      , inputText = "" }
    pure next              
  eval (UpdateInputText text next) = do
    H.modify (_ { inputText = text })
    pure next
