module Halogen where
    
import DOM

import Data.Maybe

import Control.Monad.Eff
import Control.Monad.Eff.Ref
    
import Halogen.HTML (HTML(), renderHtml)
import Halogen.Signal 
import Halogen.VirtualDOM   
 
-- | Wraps the effects required by the `runUI` and `runUIEff` functions.
type Heff eff = Eff (ref :: Ref, dom :: DOM | eff)
 
-- | Turn a non-empty `VTree`-generating signal into a `Patch`-generating signal.
-- |
-- | This function can be used to create alternative top-level handlers which use `virtual-dom`.
changes :: forall i. Signal1 i VTree -> Signal i Patch
changes s = tail s >>> differencesWith diff (head s)
 
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
-- | ui = view <$> stateful 0 (\n _ -> n + 1)
-- |   where
-- |   view :: Number -> HTML Unit
-- |   view n = button [ OnClick (const unit) ] [ text (show n) ]
-- | ```
-- |
runUI :: forall i eff. Signal1 i (HTML i) -> Heff eff Node
runUI signal = runUIEff signal (\i k -> k i)

-- | `runUIEff` is a more general version of `runUI` which can be used to construct other
-- | top-level handlers for applications.
-- |
-- | `runUIEff` takes a signal function which creates HTML documents containing _requests_, 
-- | and a handler function which accepts requests and provides new inputs to a continuation as they
-- | become available.
-- |
-- | For example, the handler function might be responsible for issuing AJAX requests on behalf of the
-- | application.
-- |
-- | In this way, all effects are pushed to the handler function at the boundary of the application.
-- |
runUIEff :: forall i r eff. Signal1 i (HTML r) -> (r -> (i -> Heff eff Unit) -> Heff eff Unit) -> Heff eff Node
runUIEff signal handler = do
  ref <- newRef Nothing
  runUI' ref
  
  where
  runUI' :: forall i. RefVal _ -> Heff eff Node
  runUI' ref = do
    let render = renderHtml requestHandler
        vtrees = render <$> signal
        diffs  = changes vtrees
        node   = createElement (head vtrees)  
    writeRef ref $ Just { signal: diffs, node: node }
    return node    
    
    where
    requestHandler :: r -> Heff eff Unit
    requestHandler r = handler r inputHandler
    
    inputHandler :: i -> Heff eff Unit
    inputHandler i = do
      Just { signal: signal, node: node } <- readRef ref
      let next = runSignal signal i
      node' <- patch (head next) node
      writeRef ref $ Just { signal: tail next, node: node' }
      