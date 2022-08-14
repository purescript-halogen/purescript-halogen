module Example.Driver.Websockets.Log where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Foreign (readString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Web.Event.Event as E
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS
import Web.Socket.ReadyState as WSS

ws_url :: String
ws_url = "wss://ws.ifelse.io"

type State =
  { messages :: Array String
  , inputText :: String
  , websocket :: Maybe WS.WebSocket
  }

data Action
  = Connect
  | LogConnectionState String
  | HandleInput String
  | ReceiveMessage String
  | Submit E.Event

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Connect
        }
    }

initialState :: forall i. i -> State
initialState _ = { messages: [], inputText: "", websocket: Nothing }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.form
    [ HE.onSubmit Submit ]
    [ HH.ol_ $ map (\msg -> HH.li_ [ HH.text msg ]) state.messages
    , HH.input
        [ HP.type_ HP.InputText
        , HP.value (state.inputText)
        , HE.onValueInput HandleInput
        ]
    , HH.button
        [ HP.type_ HP.ButtonSubmit ]
        [ HH.text "Send Message" ]
    ]

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Connect -> do
    connection <- H.liftEffect $ WS.create ws_url []
    H.modify_ (_ { websocket = Just connection })
    addWsListener connection WSET.onOpen \_ -> Just $ LogConnectionState "Open"
    addWsListener connection WSET.onClose \_ -> Just $ LogConnectionState "Closed"
    addWsListener connection WSET.onError \_ -> Just $ LogConnectionState "Error"
    addWsListener connection WSET.onMessage (map ReceiveMessage)
    where
    addWsListener :: WS.WebSocket -> E.EventType -> (Maybe String -> Maybe Action) -> H.HalogenM State Action () output m Unit
    addWsListener connection eventType messageHandler =
      void $ H.subscribe $ eventListener
        eventType
        (WS.toEventTarget connection)
        (wsMessageFromEvent >>> messageHandler)

    wsMessageFromEvent :: E.Event -> Maybe String
    wsMessageFromEvent event =
      ME.fromEvent event >>= (ME.data_ >>> readString >>> runExcept >>> hush)
  LogConnectionState connectionState -> do
    let logMessage = "Connection state: " <> connectionState
    H.modify_ \st -> st { messages = st.messages `A.snoc` logMessage }
  HandleInput text -> do
    H.modify_ (_ { inputText = text })
  ReceiveMessage msg -> do
    let incomingMessage = "Received: " <> msg
    H.modify_ \st -> st { messages = st.messages `A.snoc` incomingMessage }
  Submit ev -> do
    H.liftEffect $ E.preventDefault ev
    st <- H.get
    let outgoingMessage = st.inputText
    logMessage <- H.liftEffect $ case st.websocket of
      Just connection -> do
        connectionState <- WS.readyState connection
        case connectionState of
          WSS.Open -> do
            WS.sendString connection outgoingMessage
            pure $ "Sending: " <> outgoingMessage
          _ -> pure "Unable to send: connection not open"
      Nothing -> pure "Unable to send: connection not initialized"
    H.modify_ \st' -> st'
      { messages = st'.messages `A.snoc` logMessage
      , inputText = ""
      }
