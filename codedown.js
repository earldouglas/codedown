#!/usr/bin/env node

var marked   = require('marked');
var readline = require('readline');

if (process.argv.length === 3) {

  var source = [];

  var rl = readline.createInterface({
    terminal: false,
    input: process.stdin,
  });

  rl.on('line', function (line) {
    source.push(line);
  });

  rl.on('close', function () {

    var lang = process.argv[2];

    var renderer = new marked.Renderer();

    for (var i in renderer) {
      if ("function" === typeof renderer[i]) {
        renderer[i] = function () { return ''; };
      }
    }

    renderer.code =
      function (code, language) {
        return language === lang ? code + '\n\n' : '';
      };

    var output = marked(source.join('\n'), { renderer: renderer });
    output = output.replace(/\n+$/g, '');

    console.log(output);

  });

} else {
  console.log('usage: codedown <lang>');
  console.log('ex: codedown haskell');
}
