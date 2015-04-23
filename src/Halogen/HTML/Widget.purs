-- | This module defines helper functions for working with third-party widgets.

module Halogen.HTML.Widget where

import Data.DOM.Simple.Types

import Data.Int
import Data.Maybe
import Data.Function
import Data.Nullable

import Control.Monad.Eff

import qualified Halogen.Internal.VirtualDOM as V
    
-- | Create a `VTree` from a third-party component (or _widget_), by providing a name, an ID, and three functions:
-- | 
-- | - An initialization function, which creates the DOM node, and receives a callback function which can
-- |   be used to generate inputs
-- | - An update function, which receives the previous DOM node and optionally creates a new one.
-- | - A finalizer function, which deallocates any necessary resources when the component is removed from the DOM.
-- |
-- | The three functions share a common piece of data of a hidden type `s`.
widget :: forall eff ref i s. { ref     :: Int
                              , name    :: String
                              , id      :: String
                              , init    :: (i -> Eff eff Unit) -> Eff eff { state :: s, node :: HTMLElement }
                              , update  :: s -> HTMLElement -> Eff eff (Maybe HTMLElement)
                              , destroy :: s -> HTMLElement -> Eff eff Unit
                              } -> V.Widget eff i
widget spec = runFn6 V.widget spec.ref spec.name spec.id spec.init (\s n -> toNullable <$> spec.update s n) spec.destroy