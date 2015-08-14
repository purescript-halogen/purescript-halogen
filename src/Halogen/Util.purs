module Halogen.Util where

import Prelude

import Control.Bind ((<=<), (=<<))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (MonadEff, liftEff)

import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Document (body)
import DOM.HTML.Types (HTMLElement(), htmlElementToNode)
import DOM.Node.Node (appendChild)

appendToBody :: forall m eff. (MonadEff (dom :: DOM | eff) m) => HTMLElement -> m Unit
appendToBody e = liftEff $ do
  b <- toMaybe <$> ((body <=< document) =<< window)
  case b of
    Nothing -> pure unit
    Just b' -> void $ appendChild (htmlElementToNode e) (htmlElementToNode b')
