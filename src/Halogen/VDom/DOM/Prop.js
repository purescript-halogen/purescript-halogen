"use strict";

exports.setAttribute = function (ns, attr, val, el) {
  return function () {
    if (ns != null) {
      el.setAttributeNS(ns, attr, val);
    } else {
      el.setAttribute(attr, val);
    }
  };
};

exports.removeAttribute = function (ns, attr, el) {
  return function () {
    if (ns != null) {
      el.removeAttributeNS(ns, attr);
    } else {
      el.removeAttribute(attr);
    }
  };
};

exports.addEventListener = function (ev, listener, el) {
  return function () {
    el.addEventListener(ev, listener, false);
  };
};

exports.removeEventListener = function (ev, listener, el) {
  return function () {
    el.removeEventListener(ev, listener, false);
  };
};

exports.setProperty = function (prop, val, el) {
  return function () {
    el[prop] = val;
  };
};

exports.getProperty = function (prop, el) {
  return function () {
    return el[prop];
  };
};

exports.removeProperty = function (prop, el) {
  return function () {
    delete el[prop];
  };
};

exports.unsafeLookup = function (key, obj) {
  return obj[key];
};
