-- | The main module of the Halogen library. It defines functions for running applications
-- | assembled from the parts defined in the various submodules:
-- |
-- | - `Halogen.Signal` for responding to inputs and maintaining state
-- | - `Halogen.HTML.*` for templating HTML documents
-- | - `Halogen.Component` for building application components
-- | - `Halogen.Themes.*` for rendering using common front-end libraries
-- | - `Halogen.Mixin.*` for common additional application features
-- |
-- | The functionality of this library is completely described by the type signature of the `runUI`
-- | function, which renders a `Component` to the DOM. The other modules exist to make the construction
-- | of `Component`s as simple as possible.
-- |
module Halogen 
  ( HalogenEffects()
  , Driver()
  
  , changes
  
  , runUI
  ) where
    
import DOM

import Data.Void
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Bifunctor (rmap)

import Debug.Trace

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Control.Monad.Eff.Exception
    
import Control.Monad.Aff
    
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Renderer.VirtualDOM as R

import Halogen.Signal 
import Halogen.Internal.VirtualDOM   
import Halogen.Component   
 
-- | Wraps the effects required by the `runUI` function.
type HalogenEffects eff = (trace :: Trace, ref :: Ref, dom :: DOM | eff)
 
-- | A signal which emits patches corresponding to successive `VTree`s.
-- |
-- | This function can be used to create alternative top-level handlers which use `virtual-dom`.
changes :: VTree -> SF VTree Patch
changes = differencesWith diff

-- | This type synonym is provided to tidy up the type signature of `runUI`.
-- |
-- | The _driver function_ can be used by the caller to inject additional inputs into the system at the top-level.
-- |
-- | This is useful for supporting applications which respond to external events which originate
-- | outside the UI, such as timers or hash-change events.
-- |
-- | For example, to drive the UI with a `Tick` input every second, we might write something like the following:
-- | 
-- | ```purescript
-- | main = do
-- |   Tuple node driver <- runUI ui
-- |   appendToBody node
-- |   setInterval 1000 $ driver Tick
-- | ```
type Driver i eff = i -> Eff (HalogenEffects eff) Unit

-- | `runUI` renders a `Component` to the DOM using `virtual-dom`.
-- |
-- | This function is the workhorse of the Halogen library. It can be called in `main`
-- | to set up the application and create the driver function, which can be used to 
-- | send inputs to the UI from external components.
runUI :: forall p req eff. 
           Component (Widget (HalogenEffects eff)) (Aff (HalogenEffects eff)) R.HTML req req -> 
           Eff (HalogenEffects eff) (Tuple Node (Driver req eff))
runUI = runComponent \sf -> do
  ref <- newRef Nothing
  runUI' ref sf

-- | Internal function used in the implementation of `runUI`.
runUI' :: forall i p req eff. 
            RefVal (Maybe { signal :: SF (Either i req) Patch, node :: Node }) -> 
            SF1 (Either i req) (R.HTML (Widget (HalogenEffects eff)) (Aff (HalogenEffects eff) (Either i req))) -> 
            Eff (HalogenEffects eff) (Tuple Node (Driver req eff))
runUI' ref sf = do
  let render = R.renderHTML requestHandler id
      vtrees = render <$> sf
      diffs  = tail vtrees >>> changes (head vtrees) 
      node   = createElement (head vtrees)  
  writeRef ref $ Just { signal: diffs, node: node }
  return (Tuple node externalDriver)  
  
  where
  requestHandler :: Aff (HalogenEffects eff) (Either i req) -> Eff (HalogenEffects eff) Unit
  requestHandler aff = unsafeInterleaveEff $ runAff logger driver aff
  
  logger :: Error -> Eff (HalogenEffects eff) Unit
  logger e = trace $ "Uncaught error in asynchronous code: " <> message e
  
  driver :: Driver (Either i req) eff
  driver e = do
    Just { signal: signal, node: node } <- readRef ref
    let next = runSF signal e
    node' <- patch (head next) node
    writeRef ref $ Just { signal: tail next, node: node' }
    
  externalDriver :: Driver req eff
  externalDriver req = driver (Right req)