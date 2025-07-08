module Halogen.Aff.Util
  ( awaitLoad
  , awaitBody
  , awaitElement
  , awaitBodyFirstChild
  , selectElement
  , runHalogenAff
  , firstAndOnlyHtmlElementChildOrThrow
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, makeAff, nonCanceler, runAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throwException, error)
import Web.DOM (Element)
import Web.DOM.Element (toParentNode) as DOM.Element
import Web.DOM.ParentNode (ParentNode, QuerySelector(..), querySelector)
import Web.DOM.ParentNode as DOM.ParentNode
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
  liftEffect $ firstAndOnlyHtmlElementChildOrThrow htmlElement

-- | Tries to find an element in the document.
selectElement :: QuerySelector -> Aff (Maybe HTMLElement)
selectElement query = do
  mel <- liftEffect do
    (querySelector query <<< HTMLDocument.toParentNode <=< Window.document) =<< window
  pure $ HTMLElement.fromElement =<< mel

-- | Runs an `Aff` value of the type commonly used by Halogen components. Any
-- | unhandled errors will be re-thrown as exceptions.
runHalogenAff :: forall x. Aff x -> Effect Unit
runHalogenAff = runAff_ (either throwException (const (pure unit)))

-- | This is like `firstElementChild` but with additional safety to check that we hydrate on output of `Halogen.VDom.StringRenderer.RenderComponent.renderComponent`
-- |
-- | Suppose You have `myhtml = """<body><div id="app">""" <> Halogen.VDom.StringRenderer.RenderComponent.renderComponent myapp <> """</div></body>"""`
-- | and You give `firstAndOnlyHtmlElementChildOrThrow divWithIdAppElement`
-- | This code will check that `divWithIdAppElement` has only one child and will return this child
-- | (Why only one? Because `Halogen.VDom.StringRenderer.RenderComponent.renderComponent`
-- | doesn't take an array of components, but only one component)
firstAndOnlyHtmlElementChildOrThrow :: HTMLElement -> Effect HTMLElement
firstAndOnlyHtmlElementChildOrThrow container = do
  maybeRootElement <- map HTMLElement.fromElement $ firstAndOnlyElementChildOrThrow $ DOM.Element.toParentNode $ HTMLElement.toElement container
  case maybeRootElement of
    Just el -> pure el
    Nothing -> throwException $ error "Could not convert root element to HTMLElement type"
  where
  firstAndOnlyElementChildOrThrow :: ParentNode -> Effect Element
  firstAndOnlyElementChildOrThrow parentNode = do
    childrenCount <- DOM.ParentNode.childElementCount parentNode

    unless (childrenCount == 1) do
      throwException $ error $ "Root container should have only 1 child element (aka root element; it can be Element, Keyed, Text, etc.), but actual children count is " <> show childrenCount

    maybeRootElement <- DOM.ParentNode.firstElementChild parentNode
    case maybeRootElement of
      Just el -> pure el
      Nothing -> throwException $ error "Root element not found"
