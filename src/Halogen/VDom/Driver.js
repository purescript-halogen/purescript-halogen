"use strict";

export function createElementFromHTML(html) {
  // used: https://stackoverflow.com/a/35385518/1833322
  // alternatively: https://stackoverflow.com/a/494348/1833322
  var template = document.createElement('template');
  template.innerHTML = html;
  if (template.content.childElementCount !== 1) {
    console.error('exactly 1 html element has to be passed to rawHTML. Found: ' . html);
  } else {
    var newElement = template.content.firstChild;

    return newElement;
  }
};

export function getOuterHtml(node) {
  return node.outerHtml;
};

export function setOuterHtml(node) {
  return function(html) {
    node.outerHtml = html;
    return node;
  };
};
