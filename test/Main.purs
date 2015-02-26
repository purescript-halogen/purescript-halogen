module Test.Main where

import Data.Maybe
import Data.Tuple

import Control.Monad.Eff

import DOM

import qualified Halogen as H
import qualified Halogen.HTML as H

foreign import appendToBody
  "function appendToBody(node) {\
  \  return function() {\
  \    document.body.appendChild(node);\
  \  };\
  \}" :: forall eff. Node -> Eff (dom :: DOM | eff) Node

data ButtonEvent = Click

-- | This `Spec` defines a reusable button component, which has a single child
-- | placeholder, and supports click events.
-- | 
-- | Its state is a `Number` which represents the number of clicks
button :: H.Spec Number (Maybe H.Void)
button = H.mkSpec render foldState
  where
  render :: H.Render Number (Maybe H.Void) ButtonEvent
  render _ = H.button [H.OnClick (const Click)] [H.child Nothing]
  
  foldState :: H.FoldState Number ButtonEvent
  foldState count Click = count + 1
  
-- | This `Spec` defines a reusable label component, which supports neither child
-- | nodes nor events.
-- |
-- | Its state is anything which can be rendered as a string.
label :: forall a. (Show a) => H.Spec a H.Void
label = H.mkSpec render foldState
  where
  render :: H.Render a H.Void H.Void
  render a = H.text (show a)
  
  foldState :: H.FoldState a H.Void
  foldState st _ = st
  
-- | The UI is composed by inserting a label as the button's first (only) child, and then 
-- | placing the button next to _another_ label in a `div` element.
-- |
-- | We use the `label` component polymorphically. In the first case, it is used to render 
-- | the click count of type number. In the second case, it is used to render a string.
-- |
-- | The state type becomes a `Tuple` due to the use of the `beside` combinator. 
ui :: H.Component (Tuple Number String)
ui = (button `H.append` label) `H.beside` label

main = do
  node <- H.runComponent ui (Tuple 0 "Click the button")
  appendToBody node
