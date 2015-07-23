/* global exports, console */
"use strict";

// module Debug.Trace

exports.trace = function (a) {
  return function (b) {
    console.log(a);
    return b;
  };
};
