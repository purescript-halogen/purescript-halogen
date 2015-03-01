module Test.Main where

import Data.Maybe
import Data.Tuple

import Debug.Trace

import Control.Monad.Eff

import DOM

import Halogen
import Halogen.Signal
import Halogen.HTML 

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node

data Input = Increment | Decrement

ui :: forall eff. Signal1 (trace :: Trace | eff) Input (HTML Input)
ui = view <$> stateful 0 update
  where
  view :: Number -> HTML Input
  view n = div' [ div' [ text (show n)]
                  , div' [ button [ OnClick (const Increment) ] [ text "Increment" ]
                         , button [ OnClick (const Decrement) ] [ text "Decrement" ]
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
