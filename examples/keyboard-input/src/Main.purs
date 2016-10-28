module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)

import Data.Char as CH
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String as ST

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM

import Halogen as H
import Halogen.Query.EventSource as ES
import Halogen.HTML.Indexed as HH
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.VirtualDOM.Driver (runUI)

import Keyboard as K

type State eff = { chars :: String, unsubscribe :: Maybe (Aff (Effects eff) Unit) }

initialState :: forall eff. State eff
initialState = { chars : "", unsubscribe: Nothing }

data Query a
  = Init a
  | Append Char a
  | Clear a
  | Stop a

type Effects eff = (dom :: DOM, avar :: AVAR, keyboard :: K.KEYBOARD | eff)
type DSL eff = H.ComponentDSL (State eff) Query Void (Aff (Effects eff))

ui :: forall eff. H.Component HH.HTML Query Void (Aff (Effects eff))
ui =
  H.lifecycleComponent
    { render
    , eval
    , initialState
    , initializer: Just (H.action Init)
    , finalizer: Nothing
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
    { unsubscribe, eventSource } <- H.liftAff $ ES.eventSource' (K.onKeyUp document) \e ->
      case K.readKeyboardEvent e of
        info
          | info.shiftKey -> do
              K.preventDefault e
              let char = CH.fromCharCode info.keyCode
              pure $ Just $ H.action $ Append char
          | info.keyCode == 13 -> do
              K.preventDefault e
              pure $ Just $ H.action Clear
          | otherwise ->
              pure Nothing
    H.subscribe eventSource
    H.modify (_ { unsubscribe = Just unsubscribe })
    pure next
  eval (Append c next) = do
    H.modify (\st -> st { chars = st.chars <> ST.singleton c })
    pure next
  eval (Clear next) = do
    H.modify (_ { chars = "" })
    eval (Stop next)
  eval (Stop next) = do
    unsubscribe <- H.gets _.unsubscribe
    for_ unsubscribe \go -> H.liftAff go
    pure next

main :: Eff (H.HalogenEffects (keyboard :: K.KEYBOARD)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui body
