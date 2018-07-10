(function(root) {

  'use strict';

  var marked = root.marked || require('marked');

  var codedown = function(src, lang) {

    var renderer = new marked.Renderer();

    for (var i in renderer) {
      if ("function" === typeof renderer[i]) {
        renderer[i] = function () { return ''; };
      }
    }

    renderer.code =
      function (src, language, escaped) {
        return language === lang ? src + '\n\n' : '';
      };

    renderer.listitem = function (text) { return text; };
    renderer.list = function (body, ordered) { return body; };

    var output = marked(src, { renderer: renderer });
    output = output.replace(/\n+$/g, '');

    return output;
  };

  root.codedown = codedown;

  if (typeof module !== 'undefined' && typeof exports === 'object') {
    module.exports = codedown;
  } else if (typeof define === 'function' && define.amd) {
    define(function() { return codedown; });
  } else {
    root.codedown = codedown;
  }
})(this || (typeof window !== 'undefined' ? window : global));
