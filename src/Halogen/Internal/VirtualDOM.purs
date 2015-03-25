-- | This module provides the FFI definitions required to render HTML documents
-- | using the `virtual-dom` library.

module Halogen.Internal.VirtualDOM
  ( VTree()
  , Patch()
  , Props()
  , STProps()
  , emptyProps
  , prop
  , handlerProp
  , newProps
  , runProps
  , createElement
  , diff
  , patch
  , vtext
  , vnode
  , widget
  ) where

import DOM

import Data.Maybe
import Data.Nullable
import Data.Function

import Control.Monad.Eff
import Control.Monad.ST

-- | Virtual DOM nodes
data VTree

-- | Patch sets, used to update the DOM
data Patch

-- | Immutable property collections
data Props

-- | Mutable property collections
data STProps h

foreign import emptyProps 
  "var emptyProps = {}" :: Props

-- | Update a set of mutable properties by specifying a key/value pair
foreign import prop
  "function prop(key, value, props) {\
  \  return function() {\
  \    props[key] = value;\
  \  };\
  \}" :: forall h value eff. Fn3 String value (STProps h) (Eff (st :: ST h | eff) Unit)


-- | Update a set of mutable properties by attaching a hook for an event
foreign import handlerProp
  "function handlerProp(key, f, props) {\
  \  return function() {\
  \    var Hook = function () {};\
  \    Hook.prototype.callback = function(e) {\
  \      f(e)();\
  \    };\
  \    Hook.prototype.hook = function(node) {\
  \      node.addEventListener(key, this.callback);\
  \    };\
  \    Hook.prototype.unhook = function(node) {\
  \      node.removeEventListener(key, this.callback);\
  \    };\
  \    props['data-halogen-hook-' + key] = new Hook(f);\
  \  };\
  \}" :: forall h eff eff1 event. Fn3 String (event -> Eff eff1 Unit) (STProps h) (Eff (st :: ST h | eff) Unit)

-- | Create a new empty mutable property collection
foreign import newProps 
  "function newProps() {\
  \  return {};\
  \}" :: forall h eff. Eff (st :: ST h | eff) (STProps h)
  
-- | Freeze a mutable property collection
foreign import runProps
  "function runProps(props) {\
  \  return props();\
  \}" :: (forall h eff. Eff (st :: ST h | eff) (STProps h)) -> Props

-- | Create a DOM node from a virtual DOM tree
foreign import createElement
  "function createElement(vtree) {\
  \  return require('virtual-dom/create-element')(vtree);\
  \}" :: VTree -> Node

-- | Calculate the differences between two virtual DOM trees
foreign import diff
  "function diff(vtree1) {\
  \  return function createElement(vtree2) {\
  \    return require('virtual-dom/diff')(vtree1, vtree2);\
  \  };\
  \}" :: VTree -> VTree -> Patch
  
-- | Apply a set of patches to the DOM
foreign import patch
  "function patch(p) {\
  \  return function(node) {\
  \    return function() {\
  \      return require('virtual-dom/patch')(node, p);\
  \    };\
  \  };\
  \}" :: forall eff. Patch -> Node -> Eff (dom :: DOM | eff) Node

-- | Create a virtual DOM tree which represents a single text node
foreign import vtext 
  "function vtext(s) {\
  \  var VText = require('virtual-dom/vnode/vtext');\
  \  return new VText(s);\
  \}" :: String -> VTree

-- | Create a virtual DOM tree which represents an element with properties
foreign import vnode 
  "function vnode(name) {\
  \  return function(props) {\
  \    return function(children) {\
  \      var VirtualNode = require('virtual-dom/vnode/vnode');\
  \      return new VirtualNode(name, props, children);\
  \    };\
  \  };\
  \}" :: String -> Props -> [VTree] -> VTree
  
foreign import widgetImpl
  "function widgetImpl(name, id, init, update, destroy) {\
  \  var Widget = function () {};\
  \  Widget.prototype.type = 'Widget';\
  \  Widget.prototype.name = name;\
  \  Widget.prototype.id = id;\
  \  Widget.prototype.init = function(){\
  \    return init();\
  \  };\
  \  Widget.prototype.update = function(prev, node) {\
  \    return update(node)();\
  \  };\
  \  Widget.prototype.destroy = function(node) {\
  \    destroy(node)();\
  \  };\
  \  return new Widget();\
  \}" :: forall eff. Fn5 String String (Eff eff Node) (Node -> Eff eff (Nullable Node)) (Node -> Eff eff Unit) VTree

-- | Create a `VTree` from a third-party component (or _widget_), by providing a name, an ID, and three functions:
-- | 
-- | - An initialization function, which creates the DOM node
-- | - An update function, which receives the previous DOM node and optionally creates a new one.
-- | - A finalizer function, which deallocates any necessary resources when the component is removed from the DOM.
-- |
widget :: forall eff. String -> String -> Eff eff Node -> (Node -> Eff eff (Maybe Node)) -> (Node -> Eff eff Unit) -> VTree
widget name id init update destroy = runFn5 widgetImpl name id init (\n -> toNullable <$> update n) destroy
