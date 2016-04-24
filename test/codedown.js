'use strict';

var assert  = require('assert');
var process = require('child_process');

describe('codedown', function(){

  it('should require a <lang> argument', function (done) {
    process.exec('./codedown.js', function (err, stdout, stderr) {
      if (!err) {
        assert.equal(
          [ 'usage: codedown <lang>'
          , 'ex: codedown haskell'
          , ''
          ].join('\n'),
          stdout
        );
        done();
      } else {
        console.log(stderr);
      }
    });
  });

  it('should extract code', function (done) {
    process.exec('cat README.md | ./codedown.js haskell', function (err, stdout, stderr) {
      if (!err) {
        assert.equal(
          [ 'x :: Int'
          , 'x = 42'
          , ''
          , 'main :: IO ()'
          , 'main = putStrLn $ show x'
          , ''
          ].join('\n'),
          stdout
        );
        done();
      } else {
        console.log(stderr);
      }
    });
  });

});
