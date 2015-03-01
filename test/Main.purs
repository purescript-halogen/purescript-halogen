module Test.Main where

import Data.Maybe
import Data.Tuple

import Debug.Trace

import Control.Monad.Eff

import DOM

import Halogen
import Halogen.Signal

import qualified Halogen.HTML as H

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node

data Input = Increment | Decrement

ui :: forall eff. Signal1 (trace :: Trace | eff) Input (H.HTML Input)
ui = view <$> stateful 0 update
  where
  view :: Number -> H.HTML Input
  view n = H.div_ [ H.h1_ [ H.code_ [H.text "purescript-halogen"], H.text " demo" ]
                  , H.p_ [ H.text "Click the buttons to modify the state of the view." ]
                  , H.p_ [ H.text ("Current state: " <> show n) ]
                  , H.p_ [ H.button [ H.OnClick (const Increment) ] [ H.text "Increment" ]
                         , H.button [ H.OnClick (const Decrement) ] [ H.text "Decrement" ]
                         ]
                  ]

  update :: Number -> Input -> Eff (trace :: Trace | eff) Number
  update n Increment = do
    trace "Increment"
    return (n + 1)
  update n Decrement = do
    trace "Decrement"
    return (n - 1)

main = do
  node <- runUI ui
  appendToBody node
