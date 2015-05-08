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
  , Process()
  
  , changes
  
  , runUI
  , runUIWith
  
  , componentProcess
  , mainLoop
  ) where
    
import DOM
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Data.Maybe
import Data.Tuple
import Data.Either

import Data.Profunctor.Strong ((&&&), (***))

import Debug.Trace

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception
    
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Renderer.VirtualDOM as R

import Halogen.Signal
import Halogen.Component
import Halogen.HTML.Events.Monad 
import Halogen.Internal.VirtualDOM (VTree(), Patch(), diff, patch, createElement)

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
runUI :: forall req eff.
           Component (Event (HalogenEffects eff)) req req ->
           Eff (HalogenEffects eff) (Tuple HTMLElement (Driver req eff))
runUI sf = sf `runUIWith` \_ _ _ -> return unit

-- | A variant of `runUI` which supports a _post-render hook_. This allows applications
-- | to support third-party components or other custom behaviors by modifying the DOM after
-- | each update.
-- |
-- | This is considered an advanced feature, and should only be used with an understanding of
-- | the rendering pipeline.
runUIWith :: forall req eff.
               Component (Event (HalogenEffects eff)) req req ->
               (req -> HTMLElement -> Driver req eff -> Eff (HalogenEffects eff) Unit) -> 
               Eff (HalogenEffects eff) (Tuple HTMLElement (Driver req eff))
runUIWith sf postRender = mainLoop (pure <<< componentProcess sf postRender)

-- | A `Process` receives inputs and outputs effectful computations which update the DOM.
type Process req eff = SF (Tuple req HTMLElement) (Eff (HalogenEffects eff) HTMLElement)

-- | Build a `Process` from a `Component`.
componentProcess :: forall req eff. 
                      Component (Event (HalogenEffects eff)) req req ->
                      (req -> HTMLElement -> Driver req eff -> Eff (HalogenEffects eff) Unit) -> 
                      Driver req eff -> 
                      Tuple HTMLElement (Process req eff)
componentProcess sf postRender driver =
  let render  = R.renderHTML requestHandler
      vtrees  = render <$> sf
      diffs   = tail vtrees >>> changes (head vtrees)
      process = (diffs &&& input) *** input
      node    = createElement (head vtrees)
  in Tuple node (applyPatch <$> process)
  where
  requestHandler :: Event (HalogenEffects eff) req -> Eff (HalogenEffects eff) Unit
  requestHandler aff = runEvent logger driver aff

  logger :: Error -> Eff (HalogenEffects eff) Unit
  logger e = trace $ "Uncaught error in asynchronous code: " <> message e

  applyPatch :: Tuple (Tuple Patch req) HTMLElement -> Eff (HalogenEffects eff) HTMLElement
  applyPatch (Tuple (Tuple p req) node) = do
    node' <- patch p node
    setTimeout globalWindow 0 $ postRender req node' driver
    return node'

-- | This function provides the low-level implementation of Halogen's DOM update loop.
-- |
-- | The first argument is a function which receives the `Driver` function as an argument and
-- | constructs a `Process` which will update the DOM given an input.
-- |
-- | This function could be reused to create other types of applications based on signal functions
-- | (2D and 3D canvas, text-based, etc.)
mainLoop :: forall req eff. (Driver req eff -> Eff (HalogenEffects eff) (Tuple HTMLElement (Process req eff))) -> 
                            Eff (HalogenEffects eff) (Tuple HTMLElement (Driver req eff))
mainLoop buildProcess = do
  ref <- newRef Nothing
  go ref
  where
  go :: RefVal (Maybe { process :: Process req eff, node :: HTMLElement }) ->
        Eff (HalogenEffects eff) (Tuple HTMLElement (Driver req eff))
  go ref = do
    Tuple node process <- buildProcess driver
    writeRef ref $ Just { process: process, node: node }
    return (Tuple node driver)  
    where

    driver :: Driver req eff
    driver req = do
      ms <- readRef ref
      case ms of
        Just { process: process, node: node } -> do
          let work = runSF process (Tuple req node)
          node' <- head work
          writeRef ref $ Just { process: tail work, node: node' }
        Nothing -> trace "Error: An attempt to re-render was made during the initial render."
