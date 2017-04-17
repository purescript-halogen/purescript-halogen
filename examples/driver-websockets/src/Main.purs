module Main where

import Prelude
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Foreign (F, Foreign, toForeign, readString)
import Data.Maybe (Maybe(..))
import DOM (DOM)
import DOM.Event.EventTarget as EET
import DOM.Websocket.Event.EventTypes as WSET
import DOM.Websocket.Event.MessageEvent as ME
import DOM.Websocket.WebSocket as WS
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Log as Log

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer
  :: forall eff
   . WS.WebSocket
  -> CR.Producer String (Aff (avar :: AVAR, exception :: EXCEPTION, dom :: DOM | eff)) Unit
wsProducer socket = CRA.produce \emit ->
  EET.addEventListener
    WSET.onMessage
    (listener emit)
    false
    (WS.socketToEventTarget socket)
  where
  listener emit = EET.eventListener \ev -> do
    for_ (readHelper WS.readMessageEvent ev) \msgEvent ->
      for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
        emit (Left msg)
  readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
  readHelper read =
    either (const Nothing) Just <<< runExcept <<< read <<< toForeign

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
   . WS.WebSocket
  -> CR.Consumer Log.Message (Aff (HA.HalogenEffects (dom :: DOM | eff))) Unit
wsSender socket = CR.consumer \msg -> do
  case msg of
    Log.OutputMessage msgContents ->
      liftEff $ WS.sendString socket msgContents
  pure Nothing

main :: Eff (HA.HalogenEffects (dom :: DOM)) Unit
main = do
  connection <- WS.create (WS.URL "ws://echo.websocket.org") []
  HA.runHalogenAff do
    body <- HA.awaitBody
    io <- runUI Log.component unit body

    -- The wsSender consumer subscribes to all output messages
    -- from our component
    io.subscribe $ wsSender connection

    -- Connecting the consumer to the producer initializes both,
    -- feeding queries back to our component as messages are received.
    CR.runProcess (wsProducer connection CR.$$ wsConsumer io.query)
