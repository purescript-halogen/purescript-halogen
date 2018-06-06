module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver (runUI)
import Web.Event.Event as E
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as Web
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State = { chars :: String }

initialState :: State
initialState = { chars : "" }

data Query a
  = Init a
  | HandleKey H.SubscriptionId KeyboardEvent a

type DSL = H.HalogenM State Query () Void Aff

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  render :: forall m. State -> H.ComponentHTML Query () m
  render state =
    HH.div_
      [ HH.p_ [ HH.text "Hold down the shift key and type some characters!" ]
      , HH.p_ [ HH.text "Press ENTER or RETURN to clear and remove the event listener." ]
      , HH.p_ [ HH.text state.chars ]
      ]

  eval :: Query ~> DSL
  eval = case _ of
    Init next -> do
      document <- H.liftEffect $ Web.document =<< Web.window
      H.subscribe' \sid ->
        ES.eventListenerEventSource
          KET.keyup
          (HTMLDocument.toEventTarget document)
          (map (H.action <<< HandleKey sid) <<< KE.fromEvent)
      pure next
    HandleKey sid ev next
      | KE.shiftKey ev -> do
          H.liftEffect $ E.preventDefault (KE.toEvent ev)
          let char = KE.key ev
          when (String.length char == 1) do
            H.modify_ (\st -> st { chars = st.chars <> char })
          pure next
      | KE.key ev == "Enter" -> do
          H.liftEffect $ E.preventDefault (KE.toEvent ev)
          H.modify_ (_ { chars = "" })
          H.unsubscribe sid
          pure next
      | otherwise ->
          pure next

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
