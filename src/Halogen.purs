-- | The main module of the Halogen library. It defines functions for running applications
-- | assembled from the parts defined in the various submodules:
-- |
-- | - `Halogen.Signal` for responding to inputs and maintaining state
-- | - `Halogen.HTML.*` for templating HTML documents
-- | - `Halogen.Themes.*` for rendering using common front-end libraries
-- | - `Halogen.Mixin.*` for common additional application features
-- |
-- | The type signature and documentation for the [`runUI`](#runUI) function provides a good introduction 
-- | to this library. For more advanced use-cases, you might like to look at the `runUI` function instead.
-- |
module Halogen where
    
import DOM

import Data.Void
import Data.Maybe
import Data.Tuple
import Data.Either

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
 
-- | Wraps the effects required by the `runUI` function.
type HalogenEffects eff = (trace :: Trace, ref :: Ref, dom :: DOM | eff)
 
-- | A signal which emits patches corresponding to successive `VTree`s.
-- |
-- | This function can be used to create alternative top-level handlers which use `virtual-dom`.
changes :: VTree -> SF VTree Patch
changes = differencesWith diff
 
-- | A view is represented as a pure, non-empty signal function which
-- | consumes inputs of type `r`, and generates HTML documents.
-- |
-- | The HTML documents can contain placeholders of type `p`, and
-- | generate events which are either inputs (`i`) or requests (`r`). 
type View i p r = SF1 i (H.HTML p (Either i r)) 

-- | A pure view does not make any external requests or use placeholder elements.
type PureView i = forall p. SF1 i (H.HTML p i) 
 
-- | This type synonym is provided to tidy up the type signature of `runUI`.
-- |
-- | The _handler function_ is responsible for receiving requests from the UI, integrating with external
-- | components, and providing inputs back to the system based on the results.
-- |
-- | For example:
-- | 
-- | ```purescript
-- | data Input = SetDateAndTime DateAndTime | ...
-- | 
-- | data Request = GetDateAndTimeRequest | ...
-- | 
-- | appHandler :: forall eff. Handler Request Input eff 
-- | appHandler GetDateAndTimeRequest k =
-- |   get "/date" \response -> k (readDateAndTime response)
-- | ```
type Handler r i eff = r -> Aff (HalogenEffects eff) i
 
-- | A default renderer implementation which can be used when there are no placeholders
defaultHandler :: forall i eff. Handler Void i eff 
defaultHandler = absurd

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
 
-- | A type synonym for functions which render components to replace placeholders
type Renderer i p eff = Driver i eff -> p -> Widget (HalogenEffects eff)
 
-- | A default renderer implementation which can be used when there are no placeholders
defaultRenderer :: forall i p eff. Renderer i Void eff 
defaultRenderer _ = absurd
     
-- | A UI consists of:
-- |
-- | - A view
-- | - A handler function
-- | - A function which renders placeholder elements
type UI i p r eff = 
  { view :: View i p r
  , handler :: Handler r i eff
  , renderer :: Renderer i p eff
  } 
  
-- | A pure UI is a UI which:
-- |
-- | - Does not render placeholder elements
-- | - Does not make external requests
type PureUI i = forall eff. UI i Void Void eff
 
-- | A convenience function which can be used to construct a pure UI
pureUI :: forall i. (forall p. SF1 i (H.HTML p i)) -> PureUI i
pureUI view =
  { view: (Left <$>) <$> view
  , handler: defaultHandler
  , renderer: defaultRenderer
  }

-- | `runUI` renders a `UI` to the DOM using `virtual-dom`.
-- |
-- | This function is the workhorse of the Halogen library. It can be called in `main`
-- | to set up the application and create the driver function, which can be used to 
-- | send inputs to the UI from external components.
runUI :: forall i p r eff. UI i p r eff -> Eff (HalogenEffects eff) (Tuple Node (Driver i eff))
runUI ui = do
  ref <- newRef Nothing
  runUI' ref
  
  where
  runUI' :: RefVal _ -> Eff (HalogenEffects eff) (Tuple Node (Driver i eff))
  runUI' ref = do
    let render = R.renderHTML requestHandler (ui.renderer driver)
        vtrees = render <$> ui.view
        diffs  = tail vtrees >>> changes (head vtrees) 
        node   = createElement (head vtrees)  
    writeRef ref $ Just { signal: diffs, node: node }
    return (Tuple node driver)  
    
    where
    requestHandler :: Either i r -> Eff (HalogenEffects eff) Unit
    requestHandler (Left i) = driver i
    requestHandler (Right r) = unsafeInterleaveEff $ runAff logger driver (ui.handler r)
    
    logger :: Error -> Eff (HalogenEffects eff) Unit
    logger e = trace $ "Uncaught error in asynchronous code: " <> message e
    
    driver :: Driver i eff
    driver i = do
      Just { signal: signal, node: node } <- readRef ref
      let next = runSF signal i
      node' <- patch (head next) node
      writeRef ref $ Just { signal: tail next, node: node' }
      