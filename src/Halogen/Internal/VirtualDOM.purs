-- | This module provides the FFI definitions required to render HTML documents
-- | using the `virtual-dom` library.

module Halogen.Internal.VirtualDOM
  ( VTree()
  , Patch()
  , Props()
  , Widget()
  , emptyProps
  , prop
  , handlerProp
  , createElement
  , diff
  , patch
  , vtext
  , vnode
  , vwidget
  , widget
  ) where

import DOM

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

-- | A third-party widget
data Widget (eff :: # !)

foreign import emptyProps 
  "var emptyProps = {}" :: Props

-- | Update a set of mutable properties by specifying a key/value pair
foreign import prop
  "function prop(key, value) {\
  \  var props = {};\
  \  props[key] = value;\
  \  return props;\
  \}" :: forall value. Fn2 String value Props

-- | Update a set of mutable properties by attaching a hook for an event
foreign import handlerProp
  "function handlerProp(key, f, props) {\
  \  var props = {};\
  \  var Hook = function () {};\
  \  Hook.prototype.callback = function(e) {\
  \    f(e)();\
  \  };\
  \  Hook.prototype.hook = function(node) {\
  \    node.addEventListener(key, this.callback);\
  \  };\
  \  Hook.prototype.unhook = function(node) {\
  \    node.removeEventListener(key, this.callback);\
  \  };\
  \  props['data-halogen-hook-' + key] = new Hook(f);\
  \  return props;\
  \}" :: forall eff event. Fn2 String (event -> Eff eff Unit) Props

foreign import concatProps
  "function concatProps(p1, p2) {\
  \  var props = {};\
  \  for (var key in p1) {\
  \    if (p1.hasOwnProperty(key)) {\
  \      props[key] = p1[key];\
  \    }\
  \  }\
  \  for (var key in p2) {\
  \    if (p2.hasOwnProperty(key)) {\
  \      props[key] = p2[key];\
  \    }\
  \  }\
  \  return props;\
  \}" :: Fn2 Props Props Props

instance semigroupProps :: Semigroup Props where
  (<>) = runFn2 concatProps
  
instance monoidProps :: Monoid Props where
  mempty = emptyProps

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

-- | Create a virtual DOM tree from a `Widget`
foreign import vwidget 
  "function vwidget(w) {\
  \  return w;\
  \}" :: forall eff. Widget eff -> VTree
  
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
  \}" :: forall eff i. Fn5 String String (Eff eff Node) (Node -> Eff eff (Nullable Node)) (Node -> Eff eff Unit) (Widget eff)

-- | Create a `VTree` from a third-party component (or _widget_), by providing a name, an ID, and three functions:
-- | 
-- | - An initialization function, which creates the DOM node
-- | - An update function, which receives the previous DOM node and optionally creates a new one.
-- | - A finalizer function, which deallocates any necessary resources when the component is removed from the DOM.
-- |
widget :: forall eff i. String -> String -> Eff eff Node -> (Node -> Eff eff (Maybe Node)) -> (Node -> Eff eff Unit) -> Widget eff
widget name id init update destroy = runFn5 widgetImpl name id init (\n -> toNullable <$> update n) destroy
