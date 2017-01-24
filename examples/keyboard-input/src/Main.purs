module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)

import Data.Char as CH
import Data.Maybe (Maybe(..))
import Data.String as ST

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver (runUI)

import Keyboard as K

type State eff = { chars :: String, unsubscribe :: Maybe (Aff (Effects eff) Unit) }

initialState :: forall eff. State eff
initialState = { chars : "", unsubscribe: Nothing }

data Query a
  = Init a
  | HandleKey K.KeyboardEvent (H.SubscribeStatus -> a)

type Effects eff = (dom :: DOM, avar :: AVAR, keyboard :: K.KEYBOARD | eff)
type DSL eff = H.ComponentDSL (State eff) Query Void (Aff (Effects eff))

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

  render :: State eff -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.p_ [ HH.text "Hold down the shift key and type some characters!" ]
      , HH.p_ [ HH.text "Press ENTER or RETURN to clear and remove the event listener." ]
      , HH.p_ [ HH.text state.chars ]
      ]

  eval :: Query ~> DSL eff
  eval (Init next) = do
    document <- H.liftEff $ DOM.window >>= DOM.document <#> DOM.htmlDocumentToDocument
    H.subscribe $ ES.eventSource' (K.onKeyUp document) (Just <<< H.request <<< HandleKey)
    pure next
  eval (HandleKey e reply) =
    case K.readKeyboardEvent e of
      info
        | info.shiftKey -> do
            H.liftEff $ K.preventDefault e
            let char = CH.fromCharCode info.keyCode
            H.modify (\st -> st { chars = st.chars <> ST.singleton char })
            pure (reply H.Listening)
        | info.keyCode == 13 -> do
            H.liftEff $ K.preventDefault e
            H.modify (_ { chars = "" })
            pure (reply H.Done)
        | otherwise ->
            pure (reply H.Listening)

main :: Eff (HA.HalogenEffects (keyboard :: K.KEYBOARD)) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
