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
import Halogen.HTML.Indexed as HH
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.VirtualDOM.Driver (runUI)

import Keyboard as K

type State = { chars :: String }

initialState :: State
initialState = { chars : "" }

data Query a = Init a

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
    H.subscribe $ H.eventSource (K.onKeyUp document) \e -> do
      case K.readKeyboardEvent e of
        info
          | info.shiftKey -> do
              H.liftEff $ K.preventDefault e
              appendChar $ CH.fromCharCode info.keyCode
          | info.keyCode == 13 -> do
              H.liftEff $ K.preventDefault e
              clear
          | otherwise ->
              pure unit
    pure next

  appendChar :: Char -> H.ComponentDSL State Query Void (Aff (Effects eff)) Unit
  appendChar c = H.modify (\st -> st { chars = st.chars <> ST.singleton c })

  clear :: H.ComponentDSL State Query Void (Aff (Effects eff)) Unit
  clear = H.modify (_ { chars = "" })

main :: Eff (H.HalogenEffects (keyboard :: K.KEYBOARD)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui body
