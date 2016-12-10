"use strict";

exports.nodeRefEq = function (x) {
  return function (y) {
    return x === y;
  };
};
