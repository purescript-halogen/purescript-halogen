/* global exports */
/* jshint browser: true */
"use strict";

// module Halogen.Mixin.Router

exports.onHashChangeImpl = function (f) {
  return function () {
    window.addEventListener("hashchange", function () {
      f();
    });
  };
};

exports.getHash = function () {
  return window.location.hash;
};
