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
widget :: forall eff ctx val res. { value   :: val
                                  , name    :: String
                                  , id      :: String
                                  , init    :: (res -> Eff eff Unit) -> Eff eff { context :: ctx, node :: HTMLElement }
                                  , update  :: val -> val -> ctx -> HTMLElement -> Eff eff (Maybe HTMLElement)
                                  , destroy :: ctx -> HTMLElement -> Eff eff Unit
                                  } -> V.Widget eff res
widget spec = runFn6 V.widget 
                     spec.value 
                     spec.name 
                     spec.id 
                     spec.init 
                     (mkFn4 \v0 v1 ctx node -> toNullable <$> spec.update v0 v1 ctx node) 
                     (mkFn2 spec.destroy)