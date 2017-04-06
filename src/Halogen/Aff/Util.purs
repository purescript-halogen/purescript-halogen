module Halogen.Aff.Util
  ( awaitLoad
  , awaitBody
  , selectElement
  , runHalogenAff
  ) where

import Prelude

import Control.Monad.Aff (Aff, makeAff, runAff)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)

import Data.Maybe (Maybe(..), maybe)
import Data.Either (either)
import Data.Foreign (toForeign)

import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.HTML.Event.EventTypes (load)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement, windowToEventTarget, htmlDocumentToParentNode, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)

import Halogen.Aff.Effects (HalogenEffects)

-- | Waits for the document to load.
awaitLoad :: forall eff. Aff (dom :: DOM | eff) Unit
awaitLoad = makeAff \_ callback -> liftEff $
  window
    >>= windowToEventTarget
    >>> addEventListener load (eventListener (\_ -> callback unit)) false

-- | Waits for the document to load and then finds the `body` element.
awaitBody :: forall eff. Aff (dom :: DOM | eff) HTMLElement
awaitBody = do
  awaitLoad
  body <- selectElement (QuerySelector "body")
  maybe (throwError (error "Could not find body")) pure body

-- | Tries to find an element in the document.
selectElement
  :: forall eff
   . QuerySelector
  -> Aff (dom :: DOM | eff) (Maybe HTMLElement)
selectElement query = do
  mel <- liftEff $
    ((querySelector query <<< htmlDocumentToParentNode <=< document) =<< window)
  pure case mel of
    Nothing -> Nothing
    Just el -> either (const Nothing) Just $ runExcept $ readHTMLElement (toForeign el)

-- | Runs an `Aff` value of the type commonly used by Halogen components. Any
-- | unhandled errors will be re-thrown as exceptions.
runHalogenAff
  :: forall eff x
   . Aff (HalogenEffects eff) x
  -> Eff (HalogenEffects eff) Unit
runHalogenAff = void <<< runAff throwException (const (pure unit))
