-- | This module provides the FFI definitions required to render HTML documents
-- | using the `virtual-dom` library.

module Halogen.Internal.VirtualDOM
  ( VTree()
  , Patch()
  , Props()
  , prop
  , attr
  , handlerProp
  , initProp
  , finalizerProp
  , createElement
  , diff
  , patch
  , vtext
  , vnode
  , widget
  ) where

import Prelude

import Control.Monad.Eff (Eff())

import Data.DOM.Simple.Types (HTMLElement())
import Data.Monoid (Monoid)
import Data.Nullable (Nullable())
import Data.Function (Fn2(), runFn2)

import DOM (DOM())

-- | Virtual DOM nodes
data VTree

-- | Patch sets, used to update the DOM
data Patch

-- | Property collections
data Props

-- | Create a property from a key/value pair.
foreign import prop :: forall value. Fn2 String value Props

foreign import attr :: forall value. Fn2 String String Props

-- | Create a property from an event handler.
foreign import handlerProp :: forall eff event. Fn2 String (event -> Eff eff Unit) Props

-- | Create a property from an initializer.
foreign import initProp :: forall eff. (HTMLElement -> Eff eff Unit) -> Props

-- | Create a property from an finalizer.
foreign import finalizerProp :: forall eff. (HTMLElement -> Eff eff Unit) -> Props

foreign import concatProps :: Fn2 Props Props Props

foreign import emptyProps :: Props

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
-- | (namespace, tag name, key, properties, children).
foreign import vnode :: Nullable String -> String -> Nullable String -> Props -> Array VTree -> VTree

foreign import widget :: forall eff. Eff (dom :: DOM | eff) HTMLElement
                                  -> (HTMLElement -> Eff (dom :: DOM | eff) (Nullable HTMLElement))
                                  -> (HTMLElement -> Eff (dom :: DOM | eff) Unit)
                                  -> VTree
