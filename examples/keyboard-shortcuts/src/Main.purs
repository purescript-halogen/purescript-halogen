module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)

import Data.Char as CH
import Data.Const (Const(..))
import Data.Functor.Coproduct (Coproduct, left, right)
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

import Keyboard as K

type State = { chars :: String }

initialState :: State
initialState = { chars : "" }

data Query a
  = Init a
  | AppendChar Char a
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
      , HH.p_ [ HH.text state.chars ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (Effects eff))
  eval q =
    case q of
      Init next -> do
        document <- H.liftEff $ DOM.window >>= DOM.document <#> DOM.htmlDocumentToDocument
        let
          querySource :: H.EventSource (Coproduct (Const Unit) Query) (Aff (Effects eff))
          querySource =
            H.eventSource (K.onKeyUp document) \e -> do
              let info = K.readKeyboardEvent e
              if info.shiftKey
                 then do
                   K.preventDefault e
                   let c = CH.fromCharCode info.keyCode
                   pure $ H.action (right <<< AppendChar c)
                 else if info.keyCode == 13 then do
                   K.preventDefault e
                   pure $ H.action (right <<< Clear)
                 else do
                   pure $ left (Const unit)

        H.subscribe $ ES.catEventSource querySource
        pure next
      AppendChar c next -> do
        H.modify (\st -> st { chars = st.chars <> ST.singleton c })
        pure next
      Clear next -> do
        H.modify (_ { chars = "" })
        pure next

main :: Eff (H.HalogenEffects (keyboard :: K.KEYBOARD)) Unit
main = runHalogenAff do
  body <- awaitBody
  H.runUI ui body
