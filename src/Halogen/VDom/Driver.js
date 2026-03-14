"use strict";

export function createElementFromHTML(html) {
  var template = document.createElement('template');
  template.innerHTML = html;
  if (template.content.childElementCount !== 1) {
    throw new Error('exactly 1 html element has to be passed to rawHTML. Found: ' + html);
  }
  return template.content.firstChild;
};

export function replaceNode(oldNode, newNode) {
  oldNode.replaceWith(newNode);
};
