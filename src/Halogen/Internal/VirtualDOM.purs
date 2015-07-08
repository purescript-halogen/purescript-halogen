-- | This module provides the FFI definitions required to render HTML documents
-- | using the `virtual-dom` library.

module Halogen.Internal.VirtualDOM
  ( VTree()
  , Patch()
  , Props()
  , emptyProps
  , prop
  , handlerProp
  , initProp
  , finalizerProp
  , createElement
  , diff
  , patch
  , vtext
  , vnode
  ) where

import Prelude

import DOM
import Data.DOM.Simple.Types

import Data.Int
import Data.Maybe
import Data.Monoid
import Data.Nullable
import Data.Function

import Control.Monad.Eff
import Control.Monad.ST

-- | Virtual DOM nodes
data VTree

-- | Patch sets, used to update the DOM
data Patch

-- | Property collections
data Props

foreign import emptyProps :: Props

-- | Create a property from a key/value pair.
-- |
-- | Properties named `data-*` will be added as attributes, and any other properties will
-- | be set directly.
-- |
-- | Users should use caution when creating custom attributes, and understand how they will
-- | be added to the DOM here.
foreign import prop :: forall value. Fn2 String value Props

-- | Create a property from an event handler
foreign import handlerProp :: forall eff event. Fn2 String (event -> Eff eff Unit) Props

-- | Create a property from an initializer
foreign import initProp :: forall eff. Eff eff Unit -> Props

-- | Create a property from an finalizer
foreign import finalizerProp :: forall eff. Eff eff Unit -> Props

foreign import concatProps :: Fn2 Props Props Props

instance semigroupProps :: Semigroup Props where
  append = runFn2 concatProps

instance monoidProps :: Monoid Props where
  mempty = emptyProps

-- | Create a DOM node from a virtual DOM tree
foreign import createElement :: VTree -> HTMLElement

-- | Calculate the differences between two virtual DOM trees
foreign import diff :: VTree -> VTree -> Patch

-- | Apply a set of patches to the DOM
foreign import patch :: forall eff. Patch -> HTMLElement -> Eff (dom :: DOM | eff) HTMLElement

-- | Create a virtual DOM tree which represents a single text node
foreign import vtext :: String -> VTree


-- | Create a virtual DOM tree which represents an element with properties
foreign import vnode :: String -> Props -> Array VTree -> VTree
