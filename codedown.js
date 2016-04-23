#!/usr/bin/env node

var fs = require('fs');
var marked = require('marked');

if (process.argv.length === 4) {

  var filename = process.argv[2];
  var content = fs.readFileSync(filename, 'utf-8');

  var lang  = process.argv[3];

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

  console.log(marked(content, { renderer: renderer }));
  
} else {
  console.log('usage: codedown <markdown file> <lang>');
  console.log('ex: codedown myfile.md haskell');
}
