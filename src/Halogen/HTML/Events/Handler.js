/* global exports */
"use strict";

// module Halogen.HTML.Events.Handler

exports.preventDefaultImpl = function (e) {
  return function () {
    e.preventDefault();
  };
};

exports.stopPropagationImpl = function (e) {
  return function () {
    e.stopPropagation();
  };
};

exports.stopImmediatePropagationImpl = function (e) {
  return function () {
    e.stopImmediatePropagation();
  };
};
