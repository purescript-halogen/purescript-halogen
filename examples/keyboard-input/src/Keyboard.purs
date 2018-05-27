module Keyboard where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Web.Event.EventTarget as ET
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

onKeyUp :: HTMLDocument -> (KeyboardEvent -> Effect Unit) -> Effect (Effect Unit)
onKeyUp document fn = do
  let target = HTMLDocument.toEventTarget document
  listener <- ET.eventListener (traverse_ fn <<< KE.fromEvent)
  ET.addEventListener KET.keyup listener false target
  pure $ ET.removeEventListener KET.keyup listener false target
