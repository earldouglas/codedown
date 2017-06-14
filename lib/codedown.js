var marked    = require('marked');
var readline  = require('readline');
var minimatch = require("minimatch");

module.exports = function(src, lang, separator) {
  separator = separator || '';
  var renderer = new marked.Renderer();

  for (var i in renderer) {
    if ("function" === typeof renderer[i]) {
      renderer[i] = function () { return ''; };
    }
  }

  renderer.code =
    function (src, language, escaped) {
      return (language && minimatch(language, lang)) ? src + '\n' + separator + '\n' : '';
    };

  renderer.listitem = function (text) { return text; };
  renderer.list = function (body, ordered) { return body; };

  var output = marked(src, { renderer: renderer });
  output = output.replace(/\n+$/g, '');

  return output;
};
