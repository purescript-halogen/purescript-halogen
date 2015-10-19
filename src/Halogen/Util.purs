module Halogen.Util
  ( appendTo
  , appendToBody
  ) where

import Prelude

import Control.Bind ((<=<), (=<<))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (MonadEff, liftEff)

import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)

import DOM (DOM())
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.Event.EventTypes (load)
import DOM.Event.Types (Event())
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (HTMLElement(), htmlElementToNode, windowToEventTarget, htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.Node (appendChild)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (elementToNode)

-- | A utility for appending an `HTMLElement` to the element selected using querySelector
-- | element once the document has loaded.
appendTo :: forall m eff. (MonadEff (dom :: DOM | eff) m)  => String -> HTMLElement -> m Unit
appendTo query e = onLoad $ \_ -> do
    b <- toMaybe <$> ((querySelector query <<< htmlDocumentToParentNode <=< document) =<< window)
    case b of
      Nothing -> pure unit
      Just b' -> void $ appendChild (htmlElementToNode e) (elementToNode b')

-- | A utility for appending an `HTMLElement` to the current document's `body`
-- | element once the document has loaded.
appendToBody :: forall m eff. (MonadEff (dom :: DOM | eff) m) => HTMLElement -> m Unit
appendToBody = appendTo "body"

onLoad :: forall m eff. (MonadEff (dom :: DOM | eff) m)
       => (Event -> Eff (dom :: DOM | eff) Unit) -> m Unit
onLoad callback = liftEff $ do
  addEventListener load (eventListener callback) false <<< windowToEventTarget =<< window
