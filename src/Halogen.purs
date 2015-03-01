module Halogen 
   where
    
import DOM

import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Monoid
import Data.Exists

import Control.Monad.Eff
import Control.Monad.Eff.Ref
    
import Halogen.HTML
import Halogen.Signal 
import Halogen.VirtualDOM   
 
runUI :: forall i eff. Signal1 i (HTML i) -> Eff (ref :: Ref, dom :: DOM | eff) Node
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
      let next    = runSignal signal i
          html    = head next
          signal' = tail next
          vtree'  = renderHtml inputHandler html
          diffs   = diff vtree vtree' 
      node' <- patch diffs node
      writeRef ref $ Just { signal: signal', vtree: vtree', node: node' }
      