module Test.Main where

import Data.Tuple

import Control.Monad.Eff

import DOM

import Halogen
import Halogen.HTML

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node

render_ :: Number -> HTML Unit
render_ n = button [OnClick (const unit)] [text ("Count: " <> show n)]

foldState_ :: Number -> Unit -> Number
foldState_ n _ = n + 1

main = do
  let spec = mkSpec render_ foldState_
  node <- runSpec spec 0
  appendToBody node
