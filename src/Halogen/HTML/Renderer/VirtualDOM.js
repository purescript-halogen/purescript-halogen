/* global exports */
"use strict";

// module Halogen.HTML.Renderer.VirtualDOM

exports._eqMemoBox = function (x) {
  return function (y) {
    return x === y;
  };
};
