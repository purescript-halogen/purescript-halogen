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

// jshint maxparams: 2
exports.handlerProp = function (key, f) {
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
  props["halogen-hook-" + key] = new Hook(f);
  return props;
};

// jshint maxparams: 1
exports.initProp = function (f) {
  var hasRun = false;
  var Hook = function () {};
  Hook.prototype.hook = function (node) {
    if (!hasRun) {
      hasRun = true;
      f(node)();
    }
  };
  return { "halogen-init": new Hook(f) };
};

exports.finalizerProp = function (f) {
  var hasRun = false;
  var Hook = function () {};
  Hook.prototype.unhook = function (node) {
    if (!hasRun) {
      hasRun = true;
      f(node)();
    }
  };
  return { "halogen-final": new Hook(f) };
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
