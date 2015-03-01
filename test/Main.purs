module Test.Main where

import Data.Maybe
import Data.Tuple

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

data ButtonEvent = Click

ui :: Signal1 ButtonEvent (HTML ButtonEvent)
ui = render <$> stateful 0 (\n _ -> n + 1)
  where
  render :: Number -> HTML ButtonEvent
  render n = button [OnClick (const Click)] [text (show n)]

main = do
  node <- runUI ui
  appendToBody node
