module Halogen where
    
import DOM

import Data.Maybe
import Data.Either

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Unsafe
    
import Control.Monad.Aff
    
import Halogen.HTML (HTML(), renderHtml)
import Halogen.Signal 
import Halogen.VirtualDOM   
 
-- | Wraps the effects required by the `runUI` and `runUIEff` functions.
type HalogenEffects eff = (ref :: Ref, dom :: DOM | eff)
 
-- | A signal which emits patches corresponding to successive `VTree`s.
-- |
-- | This function can be used to create alternative top-level handlers which use `virtual-dom`.
changes :: VTree -> SF VTree Patch
changes = differencesWith diff
 
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
-- | ui :: forall eff. SF1 eff Unit (HTML Unit)
-- | ui = view <$> stateful 0 (\n _ -> n + 1)
-- |   where
-- |   view :: Number -> HTML Unit
-- |   view n = button [ onclick (const unit) ] [ text (show n) ]
-- | ```
-- |
runUI :: forall i eff. SF1 i (HTML i) -> Eff (HalogenEffects eff) Node
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
runUIEff :: forall i r eff. SF1 i (HTML r) -> (r -> (i -> Eff (HalogenEffects eff) Unit) -> Eff (HalogenEffects eff) Unit) -> Eff (HalogenEffects eff) Node
runUIEff signal handler = do
  ref <- newRef Nothing
  runUI' ref
  
  where
  runUI' :: forall i. RefVal _ -> Eff (HalogenEffects eff) Node
  runUI' ref = do
    let render = renderHtml requestHandler
        vtrees = render <$> signal
        diffs  = tail vtrees >>> changes (head vtrees) 
        node   = createElement (head vtrees)  
    writeRef ref $ Just { signal: diffs, node: node }
    return node    
    
    where
    requestHandler :: r -> Eff (HalogenEffects eff) Unit
    requestHandler r = handler r inputHandler
    
    inputHandler :: i -> Eff (HalogenEffects eff) Unit
    inputHandler i = do
      Just { signal: signal, node: node } <- readRef ref
      let next = runSF signal i
      node' <- patch (head next) node
      writeRef ref $ Just { signal: tail next, node: node' }

-- | This type class identifies those input types which support errors
class SupportsErrors input where
  liftError :: Error -> input

-- | A convenience function which uses the `Aff` monad to represent the handler function.
runUIAff :: forall i r eff. (SupportsErrors i) => SF1 i (HTML r) -> (r -> Aff (HalogenEffects eff) i) -> EffA (HalogenEffects eff) Node
runUIAff signal handler = unsafeInterleaveEff $ runUIEff signal \r k -> unsafeInterleaveEff $ runAff (k <<< liftError) k $ handler r

      