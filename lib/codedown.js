(function(root) {

  'use strict';

  var marked    = root.marked || require('marked');
  var minimatch = root.minimatch || require("minimatch");

  var codedown = function(src, lang, separator, targetSection) {

    if (separator === undefined) {
      separator = '';
    }

    separator = separator + '\n';

    var renderer = new marked.Renderer();

    var renderers =
      Object.getOwnPropertyNames(marked.Renderer.prototype);

    for (var i = 0; i < renderers.length; i++) {
      var f = renderers[i];
      if (f !== 'constructor') {
        renderer[renderers[i]] = function () { return ''; };
      }
    }

    var currentSectionNumber = [];
    var currentSectionName = null;
    var sectionNumbers = {};

    var sectionNumberMatches =
      function () {
        return currentSectionNumber.join('.').startsWith(targetSection);
      };

    var sectionNameMatches =
      function () {
        return sectionNumbers[targetSection] &&
          sectionNumbers[currentSectionName].startsWith(sectionNumbers[targetSection]);
      };

    var sectionMatches =
      function () {
        return !targetSection || sectionNumberMatches() || sectionNameMatches();
      };

    var languageMatches =
      function (language) {
        return language && (language === '*' || minimatch(language, lang));
      };

    renderer.heading =
      function(text, level, raw) {
        var index = level - 1; // 0-based indexing
        for (var i = 0; i <= index;  i++) {
          currentSectionNumber[i] = currentSectionNumber[i] || 0;
        }
        currentSectionNumber[index] = currentSectionNumber[index] + 1;
        currentSectionNumber.splice(level);

        currentSectionName = ('#'.repeat(level) + ' ' + raw).trim();
        sectionNumbers[currentSectionName] = currentSectionNumber.join('.');

        return '';
      };

    renderer.code =
      function (src, language, escaped) {

        var result = '';

        if (languageMatches(language) && sectionMatches()) {
          result = separator + src + '\n';
        }

        return result;
      };

    renderer.listitem = function (text) { return text; };
    renderer.list = function (body, ordered) { return body; };

    marked.use({ renderer: renderer });

    var output = marked.parse(src);
    output = output.replace(/\n+$/g, '');

    return output.substring(separator.length);
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
