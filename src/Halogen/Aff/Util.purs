module Halogen.Aff.Util
  ( awaitLoad
  , awaitBody
  , selectElement
  , runHalogenAff
  ) where

import Prelude

import Control.Monad.Aff (Aff, Canceler(..), makeAff, nonCanceler, runAff_)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throwException, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import DOM.Event.Types (EventType(..))
import DOM.HTML (window)
import DOM.HTML.Document (ReadyState(..), readyState)
import DOM.HTML.Event.EventTypes (load)
import DOM.HTML.Types (HTMLElement, windowToEventTarget, htmlDocumentToParentNode, readHTMLElement)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (QuerySelector(..), querySelector)
import Data.Either (Either(..), either)
import Data.Foreign (toForeign)
import Data.Maybe (Maybe(..), maybe)
import Halogen.Aff.Effects (HalogenEffects)

-- | Waits for the document to load.
awaitLoad :: forall eff. Aff (dom :: DOM | eff) Unit
awaitLoad = makeAff \callback -> liftEff do
  rs <- readyState =<< document =<< window
  case rs of
    Loading -> do
      et <- windowToEventTarget <$> window
      let listener = eventListener (\_ -> callback (Right unit))
      addEventListener (EventType "DOMContentLoaded") listener false et
      pure $ Canceler \_ -> liftEff (removeEventListener load listener false et)
    _ -> do
      callback (Right unit)
      pure nonCanceler

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
runHalogenAff = runAff_ (either throwException (const (pure unit)))
