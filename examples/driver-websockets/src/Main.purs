module Main where

import Prelude
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Var (($=))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VirtualDOM.Driver (runUI)
import Log as Log
import WebSocket as WS

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer :: forall eff. CR.Producer String (Aff (avar :: AVAR, err :: EXCEPTION, ws :: WS.WEBSOCKET | eff)) Unit
wsProducer = CRA.produce \emit -> do
  WS.Connection socket <- WS.newWebSocket (WS.URL "ws://echo.websocket.org") []
  socket.onmessage $= \event -> do
    emit $ Left $ WS.runMessage (WS.runMessageEvent event)

  -- This part would be unnecessary in the real world, but since we're just
  -- using the echo service we need to send something on init so that we have
  -- something to receive!
  socket.onopen $= \_ -> do
    socket.send (WS.Message "hello")
    socket.send (WS.Message "something")
    socket.send (WS.Message "goodbye")

-- A consumer coroutine that takes the `query` from our log component IO and
-- sends `AddMessage` queries in when it consumes messages from the producer.
wsConsumer
  :: forall eff
   . (Log.Query ~> Aff (HA.HalogenEffects (ws :: WS.WEBSOCKET | eff)))
  -> CR.Consumer String (Aff (HA.HalogenEffects (ws :: WS.WEBSOCKET | eff))) Unit
wsConsumer query = CR.consumer \msg -> do
  query $ H.action $ Log.AddMessage msg
  pure Nothing

main :: Eff (HA.HalogenEffects (ws :: WS.WEBSOCKET)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI Log.component body

  -- Connecting the consumer to the producer initializes both, opening the
  -- websocket connection and feeding messages back to our component as they
  -- are received.
  CR.runProcess (wsProducer CR.$$ wsConsumer io.query)
