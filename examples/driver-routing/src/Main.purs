module Example.Driver.Routing.Main where

import Prelude

import Data.Foldable (traverse_)
import Data.String.CodeUnits as Str
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Example.Driver.Routing.RouteLog as RouteLog
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Event.EventTarget (eventListener, addEventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.HTML.Window as Window

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI RouteLog.component unit body

  liftEffect do
    listener <- DOM.eventListener $ HCE.fromEvent >>> traverse_ \event -> do
      let hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
      launchAff_ $ void $ io.query $ H.mkTell $ RouteLog.ChangeRoute hash

    DOM.addEventListener HCET.hashchange listener false <<< Window.toEventTarget =<< DOM.window
