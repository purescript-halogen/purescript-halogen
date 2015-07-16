module Halogen.Util where

import Prelude

import Control.Bind ((>=>))
import Control.Monad.Eff (Eff())

import Data.DOM.Simple.Document (body)
import Data.DOM.Simple.Element (appendChild)
import Data.DOM.Simple.Types (HTMLElement())
import Data.DOM.Simple.Window (globalWindow, document)

import DOM (DOM())

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)
