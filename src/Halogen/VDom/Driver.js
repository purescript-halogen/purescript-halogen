"use strict";

exports.createElementFromHTML = function(html) {
  // used: https://stackoverflow.com/a/35385518/1833322
  // alternatively: https://stackoverflow.com/a/494348/1833322
  var template = document.createElement('template');
  html = html.trim(); // Never return a text node of whitespace as the result
  template.innerHTML = html;
  var newElement = template.content.firstChild;

  return newElement;
};
