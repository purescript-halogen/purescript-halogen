module Halogen.Util where

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
import DOM.HTML.Types (HTMLElement(), htmlElementToNode, windowToEventTarget)
import DOM.HTML.Window (document)
import DOM.Node.Node (appendChild)

-- | A utility for appending an `HTMLElement` to the current document's `body`
-- | element once the document has loaded.
appendToBody :: forall m eff. (MonadEff (dom :: DOM | eff) m) => HTMLElement -> m Unit
appendToBody e = liftEff $ do
  addEventListener load (eventListener onLoad) false <<< windowToEventTarget =<< window
  where
  onLoad :: forall eff. Event -> Eff (dom :: DOM | eff) Unit
  onLoad _ = do
    b <- toMaybe <$> ((body <=< document) =<< window)
    case b of
      Nothing -> pure unit
      Just b' -> void $ appendChild (htmlElementToNode e) (htmlElementToNode b')
