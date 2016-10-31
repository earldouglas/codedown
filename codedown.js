#!/usr/bin/env node

var marked   = require('marked');
var readline = require('readline');
var codedown = require('./lib/codedown.js');

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
    output = codedown(source.join('\n'), lang);
    console.log(output);
  });

} else {
  console.log('usage: codedown <lang>');
  console.log('ex: codedown haskell');
}
