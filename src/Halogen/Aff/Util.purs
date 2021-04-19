module Halogen.Aff.Util
  ( awaitLoad
  , awaitBody
  , awaitBodyFirstChild
  , awaitElement
  , awaitElementFirstChild
  , selectElement
  , findElementFirstChild
  , findElementFirstChildOrThrow
  , runHalogenAff
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either, note)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff, nonCanceler, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throwException, error)
import Halogen.VDom.Finders as Halogen.VDom.Finders
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
awaitBody = awaitElement (QuerySelector "body")

awaitBodyFirstChild :: Aff HTMLElement
awaitBodyFirstChild = awaitElementFirstChild (QuerySelector "body")

awaitElement :: QuerySelector -> Aff HTMLElement
awaitElement query = do
  awaitLoad
  body <- selectElement query
  maybe (throwError (error $ "Could not find " <> unwrap query)) pure body

awaitElementFirstChild :: QuerySelector -> Aff HTMLElement
awaitElementFirstChild query = do
  htmlElement <- awaitElement query
  findElementFirstChild htmlElement >>= either (throwError <<< error) pure

-- | Tries to find an element in the document.
selectElement :: QuerySelector -> Aff (Maybe HTMLElement)
selectElement query = do
  mel <- liftEffect $
    ((querySelector query <<< HTMLDocument.toParentNode <=< Window.document) =<< window)
  pure $ HTMLElement.fromElement =<< mel

findElementFirstChild :: HTMLElement -> Aff (Either String HTMLElement)
findElementFirstChild container = do
  element <- liftEffect $ Halogen.VDom.Finders.findElementFirstChild (HTMLElement.toElement container)
  pure $ element >>= (HTMLElement.fromElement >>> note "Could not convert root element to HTMLElement type")

findElementFirstChildOrThrow :: HTMLElement -> Aff HTMLElement
findElementFirstChildOrThrow container = findElementFirstChild container >>= either (throwError <<< error) pure

-- | Runs an `Aff` value of the type commonly used by Halogen components. Any
-- | unhandled errors will be re-thrown as exceptions.
runHalogenAff :: forall x. Aff x -> Effect Unit
runHalogenAff = runAff_ (either throwException (const (pure unit)))
