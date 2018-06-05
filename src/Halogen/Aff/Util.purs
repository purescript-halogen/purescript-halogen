module Halogen.Aff.Util
  ( awaitLoad
  , awaitBody
  , selectElement
  , runHalogenAff
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff, nonCanceler, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throwException, error)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes as ET
import Web.HTML.HTMLDocument (readyState)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

-- | Waits for the document to load.
awaitLoad :: Aff Unit
awaitLoad = makeAff \callback -> do
  rs <- readyState =<< Window.document =<< window
  case rs of
    Loading -> do
      et <- Window.toEventTarget <$> window
      listener <- eventListener (\_ -> callback (Right unit))
      addEventListener ET.domcontentloaded listener false et
      pure $ effectCanceler (removeEventListener ET.domcontentloaded listener false et)
    _ -> do
      callback (Right unit)
      pure nonCanceler

-- | Waits for the document to load and then finds the `body` element.
awaitBody :: Aff HTMLElement
awaitBody = do
  awaitLoad
  body <- selectElement (QuerySelector "body")
  maybe (throwError (error "Could not find body")) pure body

-- | Tries to find an element in the document.
selectElement :: QuerySelector -> Aff (Maybe HTMLElement)
selectElement query = do
  mel <- liftEffect $
    ((querySelector query <<< HTMLDocument.toParentNode <=< Window.document) =<< window)
  pure $ HTMLElement.fromElement =<< mel

-- | Runs an `Aff` value of the type commonly used by Halogen components. Any
-- | unhandled errors will be re-thrown as exceptions.
runHalogenAff :: forall x. Aff x -> Effect Unit
runHalogenAff = runAff_ (either throwException (const (pure unit)))
