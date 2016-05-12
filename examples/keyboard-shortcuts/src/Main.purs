module Main where

import Prelude

import Control.Monad.Aff as A
import Control.Monad.Aff.AVar as A
import Control.Monad.Eff (Eff)

import Data.Char as CH
import Data.Const
import Data.Functor.Coproduct
import Data.Maybe (Maybe(..))

import DOM as DOM
import DOM.HTML as DOM
import DOM.HTML.Window as DOM
import DOM.HTML.Types as DOM

import Halogen
import Halogen.Util (runHalogenAff, awaitBody)
import Halogen.Query.EventSource as H
import Halogen.HTML.Indexed as H

import Keyboard as K

-- | The state of the application
type State = { chars :: String }

initialState :: State
initialState = { chars : "" }

-- | Queries to the state machine
data Query a
  = Init a
  | AppendChar Char a
  | Clear a

-- | Effects embedding the Ace editor requires.
type E eff = (dom :: DOM.DOM, avar :: A.AVAR, keyboard :: K.KEYBOARD | eff)

ui :: forall eff. Component State Query (A.Aff (E eff))
ui = lifecycleComponent { render, eval, initializer, finalizer: Nothing }
  where

  initializer :: Maybe (Query Unit)
  initializer = Just (action Init)

  render :: State -> ComponentHTML Query
  render state =
    H.div_
      [ H.p_ [ H.text "Hold down the shift key and type some characters!" ]
      , H.p_ [ H.text state.chars ]
      ]

  eval :: Natural Query (ComponentDSL State Query (A.Aff (E eff)))
  eval q =
    case q of
      Init next -> do
        document <- fromEff $ DOM.window >>= DOM.document <#> DOM.htmlDocumentToDocument
        let
          querySource :: EventSource (Coproduct (Const Unit) Query) (A.Aff (E eff))
          querySource =
            eventSource (K.onKeyUp document) \e -> do
              let info = K.readKeyboardEvent e
              if info.shiftKey
                 then do
                   K.preventDefault e
                   let c = CH.fromCharCode info.keyCode
                   pure $ action (right <<< AppendChar c)
                 else if info.keyCode == 13 then do
                   K.preventDefault e
                   pure $ action (right <<< Clear)
                 else do
                   pure $ left (Const unit)

        subscribe $ H.catEventSource querySource
        pure next
      AppendChar c next -> do
        modify (\st -> st { chars = st.chars <> CH.toString c })
        pure next
      Clear next -> do
        modify (_ { chars = "" })
        pure next

main :: Eff (HalogenEffects (keyboard :: K.KEYBOARD)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui initialState body
