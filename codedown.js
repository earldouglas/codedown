#!/usr/bin/env node

var readline = require('readline');
var codedown = require('./lib/codedown.js');
var arg      = require('arg');

var args =
  arg({
    '--separator': String,
    '--section': String,
  });

if (process.argv.length >= 3) {

  var source = [];

  readline.createInterface({
    terminal: false,
    input: process.stdin,
  }).on('line', function (line) {
    source.push(line);
  }).on('close', function () {
    var lang = process.argv[2];
    var separator = args['--separator'];
    var section = args['--section'];
    output = codedown(source.join('\n'), lang, separator, section);
    console.log(output);
  });

} else {
  console.log('Usage: codedown <lang> [...]');
  console.log('');
  console.log('Options:');
  console.log('--separator <separator line>');
  console.log('--section <section number>');
  console.log('');
  console.log('Example:');
  console.log('cat README.md | codedown haskell --separator=----- --section 1.3');
}
