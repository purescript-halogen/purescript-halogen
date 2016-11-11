module Keyboard
  ( KEYBOARD
  , KeyboardEvent
  , KeyboardEventR
  , readKeyboardEvent
  , preventDefault
  , onKeyUp
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried as F
import DOM.Node.Types as DOM

foreign import data KEYBOARD :: !
foreign import addEventListenerImpl :: forall ev eff a. F.Fn3 String (ev -> Eff (keyboard :: KEYBOARD | eff) a) DOM.Document (Eff (keyboard :: KEYBOARD | eff) (Eff (keyboard :: KEYBOARD | eff) Unit))

type KeyboardEventR =
  { keyCode :: Int
  , ctrlKey :: Boolean
  , altKey :: Boolean
  , metaKey :: Boolean
  , shiftKey :: Boolean
  }

foreign import data KeyboardEvent :: *
foreign import readKeyboardEvent :: KeyboardEvent -> KeyboardEventR
foreign import preventDefault :: forall eff. KeyboardEvent -> Eff (keyboard :: KEYBOARD | eff) Unit

onKeyUp
  :: forall eff
   . DOM.Document
  -> (KeyboardEvent -> Eff (keyboard :: KEYBOARD | eff) Unit)
  -> Eff (keyboard :: KEYBOARD | eff) (Eff (keyboard :: KEYBOARD | eff) Unit)
onKeyUp document fn =
  F.runFn3 addEventListenerImpl "keyup" fn document
