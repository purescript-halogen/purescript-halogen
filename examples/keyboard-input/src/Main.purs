module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Classy.Event as DCE
import DOM.Event.KeyboardEvent as KE
import DOM.Event.Types (KeyboardEvent)
import DOM.HTML (window) as DOM
import DOM.HTML.Event.EventTypes as ET
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM
import Data.Maybe (Maybe(..))
import Data.String as String
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver (runUI)

type State = { chars :: String }

initialState :: State
initialState = { chars : "" }

data Query a
  = Init a
  | HandleKey KeyboardEvent a

type Effects eff = (dom :: DOM, avar :: AVAR | eff)
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
  eval = case _ of
    Init next -> do
      document <- H.liftEff $ DOM.window >>= DOM.document <#> DOM.htmlDocumentToDocument
      H.subscribe keyboardSubscription $
        ES.eventListenerEventSource ET.keyup document (HE.input HandleKey)
      pure next
    HandleKey ev next
      | KE.shiftKey ev -> do
          H.liftEff $ DCE.preventDefault ev
          let char = KE.key ev
          when (String.length char == 1) do
            H.modify (\st -> st { chars = st.chars <> char })
          pure next
      | KE.key ev == "Enter" -> do
          H.liftEff $ DCE.preventDefault ev
          H.modify (_ { chars = "" })
          H.unsubscribe keyboardSubscription
          pure next
      | otherwise ->
          pure next

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
