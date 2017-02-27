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
import Halogen.VDom.Driver (runUI)
import Log as Log
import WebSocket as WS
import Control.Monad.Eff.Class (liftEff)

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer
  :: forall eff
   . WS.Connection
  -> CR.Producer String (Aff (avar :: AVAR, err :: EXCEPTION, ws :: WS.WEBSOCKET | eff)) Unit
wsProducer (WS.Connection socket) = CRA.produce \emit -> do
  socket.onmessage $= \event -> do
    emit $ Left $ WS.runMessage (WS.runMessageEvent event)

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `AddMessage` queries in when it receives inputs from the
-- producer.
wsConsumer
  :: forall eff
   . (Log.Query ~> Aff (HA.HalogenEffects eff))
  -> CR.Consumer String (Aff (HA.HalogenEffects eff)) Unit
wsConsumer query = CR.consumer \msg -> do
  query $ H.action $ Log.AddMessage msg
  pure Nothing

-- A consumer coroutine that takes output messages from our component IO
-- and sends them using the websocket
wsSender
  :: forall eff
   . WS.Connection
  -> CR.Consumer Log.Message (Aff (HA.HalogenEffects (ws :: WS.WEBSOCKET | eff))) Unit    
wsSender (WS.Connection socket) = CR.consumer \msg -> do
  case msg of
    Log.OutputMessage msgContents ->
      liftEff $ socket.send (WS.Message msgContents)
  pure Nothing
                   
main :: Eff (HA.HalogenEffects (ws :: WS.WEBSOCKET)) Unit
main = do
  connection <- WS.newWebSocket (WS.URL "ws://echo.websocket.org") []
  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI Log.component unit body

    -- The wsSender consumer subscribes to all output messages
    -- from our component
    io.subscribe $ wsSender connection
        
    -- Connecting the consumer to the producer initializes both,
    -- feeding queries back to our component as messages are received.
    CR.runProcess (wsProducer connection CR.$$ wsConsumer io.query)
