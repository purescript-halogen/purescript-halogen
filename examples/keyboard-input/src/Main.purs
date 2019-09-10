module Example.KeyboardInput.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Query.Event (eventListenerEventSource)
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

data Action
  = Init
  | HandleKey H.SubscriptionId KeyboardEvent

ui :: forall f i o. H.Component f i o Aff
ui =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval (H.defaultEval { handleAction = handleAction, initialize = Just Init })
    }
  where

  render :: forall m. State -> H.ComponentHTML Action () m
  render state =
    HH.div_
      [ HH.p_ [ HH.text "Hold down the shift key and type some characters!" ]
      , HH.p_ [ HH.text "Press ENTER or RETURN to clear and remove the event listener." ]
      , HH.p_ [ HH.text state.chars ]
      ]

handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
handleAction = case _ of
  Init -> do
    document <- H.liftEffect $ Web.document =<< Web.window
    H.subscribe' \sid ->
      eventListenerEventSource
        KET.keyup
        (HTMLDocument.toEventTarget document)
        (map (HandleKey sid) <<< KE.fromEvent)
  HandleKey sid ev
    | KE.shiftKey ev -> do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        let char = KE.key ev
        when (String.length char == 1) do
          H.modify_ (\st -> st { chars = st.chars <> char })
    | KE.key ev == "Enter" -> do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        H.modify_ (_ { chars = "" })
        H.unsubscribe sid
    | otherwise ->
        pure unit

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
