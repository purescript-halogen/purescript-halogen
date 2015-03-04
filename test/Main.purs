module Test.Main where

import Data.Maybe
import Data.Tuple

import Debug.Trace

import Control.Monad.Eff

import DOM

import Halogen
import Halogen.Signal

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node

data Input = Increment | Decrement

ui :: forall eff. Signal1 Input (H.HTML Input)
ui = view <$> stateful 0 update
  where
  view :: Number -> H.HTML Input
  view n = H.div_ [ H.h1 [ A.id_ "header" ] [ H.code_ [H.text "purescript-halogen"], H.text " demo" ]
                  , H.p_ [ H.text "Click the buttons to modify the state of the view." ]
                  , H.p_ [ H.text ("Current state: " <> show n) ]
                  , H.p_ [ H.button [ A.onclick (const Increment) ] [ H.text "Increment" ]
                         , H.button [ A.onclick (const Decrement) ] [ H.text "Decrement" ]
                         ]
                  ]

  update :: Number -> Input -> Number
  update n Increment = n + 1
  update n Decrement = n - 1

main = do
  node <- runUI ui
  appendToBody node
