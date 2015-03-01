module Halogen 
   where
    
import DOM

import Data.Maybe

import Control.Monad.Eff
import Control.Monad.Eff.Ref
    
import Halogen.HTML (HTML(), renderHtml)
import Halogen.Signal 
import Halogen.VirtualDOM   
 
-- | `runUI` takes a UI represented as a signal function, and renders it to the DOM
-- | using `virtual-dom`.
-- |
-- | The signal function is responsible for rendering the HTML for the UI, and the 
-- | HTML can generate inputs which will be fed back into the signal function,
-- | resulting in DOM updates.
-- |
-- | This function returns a `Node`, and the caller is responsible for adding the node
-- | to the DOM.
-- |
-- | As a simple example, we can create a signal which responds to button clicks:
-- |
-- | ```purescript
-- | ui :: forall eff. Signal1 eff Unit (HTML Unit)
-- | ui = view <$> stateful 0 (\n _ -> pure (n + 1))
-- |   where
-- |   view :: Number -> HTML Unit
-- |   view n = button [ OnClick (const unit) ] [ text (show n) ]
-- | ```
runUI :: forall i eff. Signal1 (ref :: Ref, dom :: DOM | eff) i (HTML i) -> Eff (ref :: Ref, dom :: DOM | eff) Node
runUI signal = do
  ref <- newRef Nothing
  runUI' ref
  where
  runUI' :: forall i. RefVal _ -> Eff (ref :: Ref, dom :: DOM | eff) Node
  runUI' ref = do
    let html  = head signal
        next  = tail signal
        vtree = renderHtml inputHandler html
        node  = createElement vtree  
    writeRef ref $ Just { signal: next, vtree: vtree, node: node }
    return node    
    where
    inputHandler :: i -> Eff (ref :: Ref, dom :: DOM | eff) Unit
    inputHandler i = do
      Just { signal: signal, vtree: vtree, node: node } <- readRef ref
      next <- runSignal signal i
      let html    = head next
          signal' = tail next
          vtree'  = renderHtml inputHandler html
          diffs   = diff vtree vtree' 
      node' <- patch diffs node
      writeRef ref $ Just { signal: signal', vtree: vtree', node: node' }
      