-- | This module provides the FFI definitions required to render HTML documents
-- | using the `virtual-dom` library.

module Halogen.Internal.VirtualDOM
  ( VTree()
  , Patch()
  , VProps()
  , emptyVProps
  , vprop
  , attrVProp
  , nsAttrVProp
  , handlerVProp
  , initVProp
  , finalizerVProp
  , createElement
  , diff
  , patch
  , vtext
  , vnode
  ) where

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
data VProps

foreign import emptyVProps
  "var emptyVProps = {}" :: VProps

-- | Create a property from a key/value pair.
-- |
-- | Properties named `data-*` will be added as attributes, and any other properties will
-- | be set directly.
-- |
-- | Users should use caution when creating custom attributes, and understand how they will
-- | be added to the DOM here.
foreign import vprop
  """
  function vprop(key, value) {
    var props = {};
    props[key] = value;
    return props;
  }
  """ :: forall value. Fn2 String value VProps

foreign import attrVProp
  """
  function attrVProp(key, value) {
    var attrs = {};
    attrs[key] = value;
    return {attributes: attrs};
  }
  """ :: forall value. Fn2 String value VProps

foreign import nsAttrVProp
  """
  var nsAttrVProp = (function() {
    var attributeHook = require('virtual-dom/virtual-hyperscript/hooks/attribute-hook');
    return function nsAttrVProp(ns, key, value) {
      var props = {};
      props[key] = attributeHook(ns || null, value);
      return props;
    };
  })();
  """ :: forall value. Fn3 String String value VProps

-- | Create a property from an event handler
foreign import handlerVProp
  """
  function handlerVProp(key, f) {
    var props = {};
    var Hook = function () {};
    Hook.prototype.callback = function (e) {
      f(e)();
    };
    Hook.prototype.hook = function (node) {
      node.addEventListener(key, this.callback);
    };
    Hook.prototype.unhook = function (node) {
      node.removeEventListener(key, this.callback);
    };
    props['halogen-hook-' + key] = new Hook(f);
    return props;
  }
  """ :: forall eff event. Fn2 String (event -> Eff eff Unit) VProps

-- | Create a property from an initializer
foreign import initVProp
  """
  function initVProp(f) {
    var props = {};
    var Hook = function () {};
    Hook.prototype.hook = function (node, prop, prev) {
      if (typeof prev === 'undefined') {
        f();
      };
    };
    props['halogen-init'] = new Hook(f);
    return props;
  }
  """ :: forall eff. Eff eff Unit -> VProps

-- | Create a property from an finalizer
foreign import finalizerVProp
  """
  function finalizerVProp(f) {
    var props = {};
    var Hook = function () {};
    Hook.prototype.hook = function () { };
    Hook.prototype.unhook = function () {
      f();
    };
    props['halogen-finalizer'] = new Hook(f);
    return props;
  }
  """ :: forall eff. Eff eff Unit -> VProps

foreign import concatVProps
  """
  function concatVProps(p1, p2) {
    var attrs = {};
    var props = {};
    for (var key in p1.attributes || null) {
      attrs[key] = p1.attributes[key];
    }
    for (var key in p2.attributes || null) {
      attrs[key] = p2.attributes[key];
    }
    for (var key in p1) {
      props[key] = p1[key];
    }
    for (var key in p2) {
      props[key] = p2[key];
    }
    props.attributes = attrs;
    return props;
  }
  """ :: Fn2 VProps VProps VProps

instance semigroupVProps :: Semigroup VProps where
  (<>) = runFn2 concatVProps

instance monoidVProps :: Monoid VProps where
  mempty = emptyVProps

-- | Create a DOM node from a virtual DOM tree
foreign import createElement
  """
  var createElement = (function () {
    var vcreateElement = require('virtual-dom/create-element');
    return function (vtree) {
      return vcreateElement(vtree);
    };
  }());
  """ :: VTree -> HTMLElement

-- | Calculate the differences between two virtual DOM trees
foreign import diff
  """
  var diff = (function () {
    var vdiff = require('virtual-dom/diff');
    return function (vtree1) {
      return function (vtree2) {
        return vdiff(vtree1, vtree2);
      };
    };
  }());

  """ :: VTree -> VTree -> Patch

-- | Apply a set of patches to the DOM
foreign import patch
  """
  var patch = (function () {
    var vpatch = require('virtual-dom/patch');
    return function (p) {
      return function (node) {
        return function () {
          return vpatch(node, p);
        };
      };
    };
  }());
  """ :: forall eff. Patch -> HTMLElement -> Eff (dom :: DOM | eff) HTMLElement

-- | Create a virtual DOM tree which represents a single text node
foreign import vtext
  """
  var vtext = (function () {
    var VText = require('virtual-dom/vnode/vtext');
    return function (s) {
      return new VText(s);
    };
  }());
  """ :: String -> VTree


-- | Create a virtual DOM tree which represents an element with properties
foreign import vnode
  """
  var vnode = (function () {
    var VirtualNode = require('virtual-dom/vnode/vnode');
    var SoftSetHook = require('virtual-dom/virtual-hyperscript/hooks/soft-set-hook');
    return function (name) {
      return function (attr) {
        return function (children) {
          var props = {
            attributes: attr.attributes || {}
          };
          for (var key in attr) {
            if ((key.indexOf('data-') === 0) || (key === 'readonly')) {
              props.attributes[key] = attr[key];
            } else {
              props[key] = attr[key];
            }
          }
          if (name === 'input' && props.value !== undefined) {
            props.value = new SoftSetHook(props.value);
          }
          return new VirtualNode(name, props, children, attr.key);
        };
      };
    };
  }());
  """ :: String -> VProps -> [VTree] -> VTree
