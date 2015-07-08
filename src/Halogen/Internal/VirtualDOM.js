/* global exports, require */
"use strict";

// module Halogen.Internal.VirtualDOM

exports.emptyProps = {};

// jshint maxparams: 2
exports.prop = function (key, value) {
  var props = {};
  props[key] = value;
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
  var props = {};
  var Hook = function () {};
  // jshint maxparams: 3
  Hook.prototype.hook = function (node, prop, prev) {
    if (typeof prev === "undefined") f();
  };
  props["halogen-init"] = new Hook(f);
  return props;
};

exports.finalizerProp = function (f) {
  var props = {};
  var Hook = function () {};
  Hook.prototype.hook = function () { };
  Hook.prototype.unhook = function () {
    f();
  };
  props["halogen-finalizer"] = new Hook(f);
  return props;
};

// jshint maxparams: 2
exports.concatProps = function (p1, p2) {
  var props = {};
  var key;
  for (key in p1) {
    if (p1.hasOwnProperty(key)) props[key] = p1[key];
  }
  for (key in p2) {
    if (p2.hasOwnProperty(key)) props[key] = p2[key];
  }
  return props;
};

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
  return function (name) {
    return function (attr) {
      return function (children) {
        var props = {
          attributes: {}
        };
        for (var key in attr) {
          if (key.indexOf("data-") === 0 || key === "readonly") {
            props.attributes[key] = attr[key];
          } else {
            props[key] = attr[key];
          }
        }
        if (name === "input" && props.value !== undefined) {
          props.value = new SoftSetHook(props.value);
        }
        return new VirtualNode(name, props, children, attr.key);
      };
    };
  };
}();
