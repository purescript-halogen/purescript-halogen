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

render :: Render Number Unit
render n = button [OnClick (const unit)] [text ("Count: " <> show n)]

foldState :: FoldState Number Unit
foldState n _ = n + 1

main = do
  let spec = mkSpec render foldState
  node <- runSpec spec 0
  appendToBody node
