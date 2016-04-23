#!/usr/bin/env node

var marked   = require('marked');
var readline = require('readline');

if (process.argv.length === 3) {

  var lines = [];

  var rl = readline.createInterface({
    input: process.stdin,
  });

  rl.on('line', function (line) {
    lines.push(line);
  });

  rl.on('close', function () {

    var lang = process.argv[2];

    var renderer = new marked.Renderer();

    renderer.code = function (code, language) {
      return language === lang ? code + '\n\n' : '';
    };

    var nope = function () { return ''; };

    renderer.blockquote  = nope;
    renderer.br          = nope;
    renderer.codespan    = nope;
    renderer.del         = nope;
    renderer.em          = nope;
    renderer.heading     = nope;
    renderer.hr          = nope;
    renderer.html        = nope;
    renderer.image       = nope;
    renderer.link        = nope;
    renderer.list        = nope;
    renderer.listitem    = nope;
    renderer.paragraph   = nope;
    renderer.strong      = nope;
    renderer.table       = nope;
    renderer.tablecell   = nope;
    renderer.tablerow    = nope;
    renderer.text        = nope;

    var content = lines.join('\n');
    console.log(marked(content, { renderer: renderer }));

  });

} else {
  console.log('usage: codedown <lang>');
  console.log('ex: codedown haskell');
}
