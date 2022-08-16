module Example.Driver.Websockets.Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Example.Driver.Websockets.Log as Log
import Foreign (readString)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Query.Event as HQE
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Web.Event.Event as E
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as WSME
import Web.Socket.WebSocket as WS

main :: Effect Unit
main = do
  connection <- WS.create "wss://ws.ifelse.io" []
  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI Log.component unit body

    -- Subscribe to all output messages from our component,
    -- forwarding them through the websocket
    let forwardToWebsocket = case _ of
          Log.OutputMessage message -> WS.sendString connection message
    _ <- H.liftEffect $ HS.subscribe io.messages forwardToWebsocket

    -- Subscribe to messages from the websocket,
    -- forwarding them to the child component
    let websocketEmitter =
          HQE.eventListener WSET.onMessage (WS.toEventTarget connection) wsMessageFromEvent
        forwardToChild =
          launchAff_ <<< void <<< io.query <<< H.mkTell <<< Log.ReceiveMessage
    void $ H.liftEffect $ HS.subscribe websocketEmitter forwardToChild

wsMessageFromEvent :: E.Event -> Maybe String
wsMessageFromEvent event = do
  wsEvent <- WSME.fromEvent event
  let foreignPayload = WSME.data_ wsEvent
  foreignPayload # (readString >>> runExcept >>> hush)
