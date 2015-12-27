module Halogen.Util
  ( appendTo
  , appendToBody
  , onLoad
  ) where

import Prelude (..)

import Control.Bind ((<=<), (=<<))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (class MonadEff, liftEff)

import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)

import DOM (DOM())
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.Event.EventTypes (load)
import DOM.HTML (window)
import DOM.HTML.Types (HTMLElement(), htmlElementToNode, windowToEventTarget, htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.Node (appendChild)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (elementToNode)

-- | A utility for appending an `HTMLElement` to the element selected using querySelector
-- | element (synchronously).
appendTo :: forall m eff. (MonadEff (dom :: DOM | eff) m)
         => String -> HTMLElement -> m Unit
appendTo query elem = liftEff $ do
    b <- toMaybe <$> ((querySelector query <<< htmlDocumentToParentNode <=< document) =<< window)
    case b of
      Nothing -> pure unit
      Just b' -> void $ appendChild (htmlElementToNode elem) (elementToNode b')

-- | A utility for appending an `HTMLElement` to the current document's `body`
-- | element once the document has loaded.
appendToBody :: forall m eff. (MonadEff (dom :: DOM | eff) m)
             => HTMLElement -> m Unit
appendToBody = appendTo "body"

-- | On load, discard the onLoad event and call a synchronous action.
onLoad :: forall m eff. (MonadEff (dom :: DOM | eff) m)
       => Eff (dom :: DOM | eff) Unit -> m Unit
onLoad callback = liftEff $ do
  addEventListener load (eventListener (\_ -> callback)) false <<< windowToEventTarget =<< window
