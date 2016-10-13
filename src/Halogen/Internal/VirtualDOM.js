/* global exports, require */
"use strict";

// module Halogen.Internal.VirtualDOM

var vcreateElement = require("virtual-dom/create-element");
var vdiff = require("virtual-dom/diff");
var vpatch = require("virtual-dom/patch");
var VText = require("virtual-dom/vnode/vtext");
var VirtualNode = require("virtual-dom/vnode/vnode");
var SoftSetHook = require("virtual-dom/virtual-hyperscript/hooks/soft-set-hook");

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

exports.refPropImpl = function (nothing) {
  return function (just) {

    var ifHookFn = function (init) {
      // jshint maxparams: 3
      return function (node, prop, diff) {
        // jshint validthis: true
        if (typeof diff === "undefined") {
          this.f(init ? just(node) : nothing)();
        }
      };
    };

    // jshint maxparams: 1
    function RefHook (f) {
      this.f = f;
    }

    RefHook.prototype = {
      hook: ifHookFn(true),
      unhook: ifHookFn(false)
    };

    return function (f) {
      return { "halogen-ref": new RefHook(f) };
    };
  };
};

// jshint maxparams: 3
function HalogenWidget (tree, eq, render) {
  this.tree = tree;
  this.eq = eq;
  this.render = render;
  this.vdom = null;
  this.el = null;
}

HalogenWidget.prototype = {
  type: "Widget",
  init: function () {
    this.vdom = this.render(this.tree);
    this.el = vcreateElement(this.vdom);
    return this.el;
  },
  update: function (prev, node) {
    if (!prev.tree || !this.eq(prev.tree.slot)(this.tree.slot)) {
      return this.init();
    }
    if (this.tree.thunk) {
      this.vdom = prev.vdom;
      this.el = prev.el;
    } else {
      this.vdom = this.render(this.tree);
      try {
        this.el = vpatch(node, vdiff(prev.vdom, this.vdom));
      } catch (e) {
      }
    }
  }
};

exports.widget = function (tree) {
  return function (eq) {
    return function (render) {
      return new HalogenWidget(tree, eq, render);
    };
  };
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

exports.createElement = function (vtree) {
  return vcreateElement(vtree);
};

exports.diff = function (vtree1) {
  return function (vtree2) {
    vtree1 = vtree1 || {};
    return vdiff(vtree1, vtree2);
  };
};

exports.patch = function (p) {
  return function (node) {
    return function () {
      var res = node;
      try {
        res = vpatch(node, p);
      } catch (e) {
      }
      return res;
    };
  };
};

exports.vtext = function (s) {
  return new VText(s);
};

exports.vnode = function (namespace) {
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
