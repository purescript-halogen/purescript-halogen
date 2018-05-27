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
import Keyboard as K
import Web.Event.Event as E
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

type State = { chars :: String }

initialState :: State
initialState = { chars : "" }

data Query a
  = Init a
  | HandleKey KeyboardEvent (H.SubscribeStatus -> a)

type DSL = H.ComponentDSL State Query Void Aff

ui :: H.Component HH.HTML Query Unit Void Aff
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

  eval :: Query ~> DSL
  eval (Init next) = do
    document <- H.liftEffect $ DOM.document =<< DOM.window
    H.subscribe $ ES.eventSource' (K.onKeyUp document) (Just <<< H.request <<< HandleKey)
    pure next
  eval (HandleKey ev reply)
    | KE.shiftKey ev = do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        let char = KE.key ev
        when (String.length char == 1) do
          H.modify_ (\st -> st { chars = st.chars <> char })
        pure (reply H.Listening)
    | KE.key ev == "Enter" = do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        H.modify_ (_ { chars = "" })
        pure (reply H.Done)
    | otherwise =
        pure (reply H.Listening)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
