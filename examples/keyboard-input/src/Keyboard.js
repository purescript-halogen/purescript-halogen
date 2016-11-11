/* global exports */
"use strict";

exports.addEventListenerImpl = function(eventName, fn, element) {
    return function() {
        var handler = function(e) {
            fn(e)();
        };
        element.addEventListener(eventName, handler);
        return function () {
          element.removeEventListener(eventName, handler);
        }
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
