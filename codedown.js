#!/usr/bin/env node

var marked   = require('marked');
var readline = require('readline');
var codedown = require('./lib/codedown.js');

if (process.argv.length >= 3) {

  var source = [];

  readline.createInterface({
    terminal: false,
    input: process.stdin,
  }).on('line', function (line) {
    source.push(line);
  }).on('close', function () {
    var lang = process.argv[2];
    var separator = process.argv[3];
    output = codedown(source.join('\n'), lang, separator);
    console.log(output);
  });

} else {
  console.log('usage: codedown <lang> [<separator>]');
  console.log('ex: codedown haskell');
}
