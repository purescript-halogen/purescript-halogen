module Halogen.Util where

import Prelude

import Control.Bind ((>=>))
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Class (MonadEff, liftEff)

import Data.DOM.Simple.Document (body)
import Data.DOM.Simple.Element (appendChild)
import Data.DOM.Simple.Types (HTMLElement())
import Data.DOM.Simple.Window (globalWindow, document)

import DOM (DOM())

appendToBody :: forall m eff. (MonadEff (dom :: DOM | eff) m) => HTMLElement -> m Unit
appendToBody e = liftEff $ document globalWindow >>= (body >=> flip appendChild e)
