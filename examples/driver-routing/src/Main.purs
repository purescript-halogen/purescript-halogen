module Main where

import Prelude
import Control.Coroutine as CR
import Control.Coroutine.Aff as CRA
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..))
import Data.String as Str
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Event.EventTypes as ET
import DOM.HTML.Event.HashChangeEvent as HCE
import DOM.HTML.Event.Types (HashChangeEvent, readHashChangeEvent) as DOM
import DOM.HTML.Types (windowToEventTarget) as DOM
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import RouteLog as RouteLog

-- A producer coroutine that emits messages whenever the window emits a
-- `hashchange` event.
hashChangeProducer
  :: forall eff
   . CR.Producer DOM.HashChangeEvent (Aff (avar :: AVAR, dom :: DOM | eff)) Unit
hashChangeProducer = CRA.produce \emit ->
  let
    emitter e =
      case runExcept (DOM.readHashChangeEvent (toForeign e)) of
        Left _ -> pure unit
        Right hce -> emit (Left hce)
  in
    liftEff $
      DOM.window
        >>= DOM.windowToEventTarget
        >>> DOM.addEventListener ET.hashchange (DOM.eventListener emitter) false

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ChangeRoute` queries in when it receives inputs from the
-- producer.
hashChangeConsumer
  :: forall eff
   . (RouteLog.Query ~> Aff (HA.HalogenEffects eff))
  -> CR.Consumer DOM.HashChangeEvent (Aff (HA.HalogenEffects eff)) Unit
hashChangeConsumer query = CR.consumer \event -> do
  let hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
  query $ H.action $ RouteLog.ChangeRoute hash
  pure Nothing

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI RouteLog.component unit body

  -- Connecting the consumer to the producer initializes both, adding the event
  -- listener to the window and feeding queries back to our component as events
  -- are received.
  CR.runProcess (hashChangeProducer CR.$$ hashChangeConsumer io.query)
