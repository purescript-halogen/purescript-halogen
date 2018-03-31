module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
import Data.Char as CH
import Data.Maybe (Maybe(..))
import Data.String as ST
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver (runUI)
import Keyboard as K

type State = { chars :: String }

initialState :: State
initialState = { chars : "" }

data Query a
  = Init a
  | HandleKey K.KeyboardEvent a

type Effects eff = (dom :: DOM, avar :: AVAR, keyboard :: K.KEYBOARD | eff)
type DSL eff = H.ComponentDSL State Query Void (Aff (Effects eff))

keyboardSubscription :: H.SubscriptionId
keyboardSubscription = H.SubscriptionId "keyboard"

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (Effects eff))
ui =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.p_ [ HH.text "Hold down the shift key and type some characters!" ]
      , HH.p_ [ HH.text "Press ENTER or RETURN to clear and remove the event listener." ]
      , HH.p_ [ HH.text state.chars ]
      ]

  eval :: Query ~> DSL eff
  eval (Init next) = do
    document <- H.liftEff $ DOM.window >>= DOM.document <#> DOM.htmlDocumentToDocument
    H.subscribe keyboardSubscription $ ES.effEventSource \emitter -> do
      removeListener <- K.onKeyUp document \ev -> ES.emit emitter (H.action (HandleKey ev))
      pure $ ES.Finalizer removeListener
    pure next
  eval (HandleKey e next) = do
    case K.readKeyboardEvent e of
      info
        | info.shiftKey -> do
            H.liftEff $ K.preventDefault e
            let char = CH.fromCharCode info.keyCode
            H.modify (\st -> st { chars = st.chars <> ST.singleton char })
        | info.keyCode == 13 -> do
            H.liftEff $ K.preventDefault e
            H.modify (_ { chars = "" })
            H.unsubscribe keyboardSubscription
        | otherwise ->
            pure unit
    pure next

main :: Eff (HA.HalogenEffects (keyboard :: K.KEYBOARD)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
