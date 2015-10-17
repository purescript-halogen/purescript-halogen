/* global exports, require */
"use strict";

// module Halogen.Internal.VirtualDOM

// jshint maxparams: 2
exports.prop = function (key, value) {
  var props = {};
  props[key] = value;
  return props;
};

// jshint maxparams: 2
exports.attr = function (key, value) {
  var props = { attributes: {} };
  props.attributes[key] = value;
  return props;
};

function HandlerHook (key, f) {
  this.key = key;
  this.callback = function (e) {
    f(e)();
  };
}

HandlerHook.prototype = {
  hook: function (node) {
    node.addEventListener(this.key, this.callback);
  },
  unhook: function (node) {
    node.removeEventListener(this.key, this.callback);
  }
};

// jshint maxparams: 2
exports.handlerProp = function (key, f) {
  var props = {};
  props["halogen-hook-" + key] = new HandlerHook(key, f);
  return props;
};

// jshint maxparams: 3
function ifHookFn (node, prop, diff) {
  // jshint validthis: true
  if (typeof diff === "undefined") {
    this.f(node)();
  }
}

// jshint maxparams: 1
function InitHook (f) {
  this.f = f;
}

InitHook.prototype = {
  hook: ifHookFn
};

exports.initProp = function (f) {
  return { "halogen-init": new InitHook(f) };
};

function FinalHook (f) {
  this.f = f;
}

FinalHook.prototype = {
  unhook: ifHookFn
};

exports.finalizerProp = function (f) {
  return { "halogen-final": new FinalHook(f) };
};

exports.concatProps = function () {
  // jshint maxparams: 2
  var hOP = Object.prototype.hasOwnProperty;
  var copy = function (props, result) {
    for (var key in props) {
      if (hOP.call(props, key)) {
        if (key === "attributes") {
          var attrs = props[key];
          var resultAttrs = result[key] || (result[key] = {});
          for (var attr in attrs) {
            if (hOP.call(attrs, attr)) {
              resultAttrs[attr] = attrs[attr];
            }
          }
        } else {
          result[key] = props[key];
        }
      }
    }
    return result;
  };
  return function (p1, p2) {
    return copy(p2, copy(p1, {}));
  };
}();

exports.emptyProps = {};

exports.createElement = function () {
  var vcreateElement = require("virtual-dom/create-element");
  return function (vtree) {
    return vcreateElement(vtree);
  };
}();

exports.diff = function () {
  var vdiff = require("virtual-dom/diff");
  return function (vtree1) {
    return function (vtree2) {
      return vdiff(vtree1, vtree2);
    };
  };
}();

exports.patch = function () {
  var vpatch = require("virtual-dom/patch");
  return function (p) {
    return function (node) {
      return function () {
        return vpatch(node, p);
      };
    };
  };
}();

exports.vtext = function () {
  var VText = require("virtual-dom/vnode/vtext");
  return function (s) {
    return new VText(s);
  };
}();

exports.vnode = function () {
  var VirtualNode = require("virtual-dom/vnode/vnode");
  var SoftSetHook = require("virtual-dom/virtual-hyperscript/hooks/soft-set-hook");
  return function (namespace) {
    return function (name) {
      return function (key) {
        return function (props) {
          return function (children) {
            if (name === "input" && props.value !== undefined) {
              props.value = new SoftSetHook(props.value);
            }
            return new VirtualNode(name, props, children, key, namespace);
          };
        };
      };
    };
  };
}();
