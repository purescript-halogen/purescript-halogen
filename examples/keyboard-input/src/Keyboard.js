/* global exports */
"use strict";

// module Keyboard
exports.addEventListenerImpl = function(eventName, fn, element) {
    return function() {
        element.addEventListener(eventName, function(e) {
            fn(e)();
        });
    };
};

exports.readKeyboardEvent = function(e) {
    return e;
};

exports.preventDefault = function(e) {
    return function() {
        e.preventDefault();
    };
};
