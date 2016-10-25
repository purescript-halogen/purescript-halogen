module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)

import Data.Char as CH
import Data.Functor.Coproduct (Coproduct, right, left)
import Data.Const (Const(..))
import Data.Maybe (Maybe(..))
import Data.String as ST

import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument) as DOM
import DOM.HTML.Window (document) as DOM

import Halogen as H
import Halogen.HTML.Indexed as HH
import Halogen.Query.EventSource as ES
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.VirtualDOM.Driver (runUI)

import Keyboard as K

type State = { chars :: String }

initialState :: State
initialState = { chars : "" }

data Query a
  = Init a
  | Append Char a
  | Clear a

type Effects eff = (dom :: DOM, avar :: AVAR, keyboard :: K.KEYBOARD | eff)

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

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.p_ [ HH.text "Hold down the shift key and type some characters!" ]
      , HH.p_ [ HH.text "Press ENTER or RETURN to clear." ]
      , HH.p_ [ HH.text state.chars ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (Effects eff))
  eval (Init next) = do
    document <- H.liftEff $ DOM.window >>= DOM.document <#> DOM.htmlDocumentToDocument
    let
      source :: H.EventSource (Coproduct (Const Unit) Query) (Aff (Effects eff))
      source =
        H.eventSource (K.onKeyUp document) \e ->
          case K.readKeyboardEvent e of
            info
              | info.shiftKey -> do
                  K.preventDefault e
                  let char = CH.fromCharCode info.keyCode
                  pure $ right $ H.action $ Append char
              | info.keyCode == 13 -> do
                  K.preventDefault e
                  pure $ right $ H.action Clear
              | otherwise ->
                  pure $ left (Const unit)
    H.subscribe $ ES.catEventSource source
    pure next
  eval (Append c next) = do
    H.modify (\st -> st { chars = st.chars <> ST.singleton c })
    pure next
  eval (Clear next) = do
    H.modify (_ { chars = "" })
    pure next

main :: Eff (H.HalogenEffects (keyboard :: K.KEYBOARD)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui body
