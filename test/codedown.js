'use strict';

var assert  = require('assert');
var process = require('child_process');

describe('codedown', function(){

  it('should require a <lang> argument', function (done) {
    process.exec('./codedown.js', function (err, stdout, stderr) {
      if (!err) {
        assert.equal(
          [ 'usage: codedown <lang> [<separator>]'
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
          stdout,
          [ 'x :: Int'
          , 'x = 42'
          , ''
          , 'main :: IO ()'
          , 'main = putStrLn $ show x'
          , ''
          ].join('\n')
        );
        done();
      } else {
        console.log(stderr);
      }
    });
  });

  it('should extract code with wildcard', function (done) {
    process.exec('cat README.md | ./codedown.js "**/*.js"', function (err, stdout, stderr) {
      if (!err) {
        assert.equal(
          stdout,
          'var x = 42;\n'
        );
        done();
      } else {
        console.log(stderr);
      }
    });
  });

  it('should extract code with separator', function (done) {
    process.exec('cat README.md | ./codedown.js java -----', function (err, stdout, stderr) {
      if (!err) {
        assert.equal(
          stdout,
          [ 'System.out.println("hello")'
          , '-----'
          , 'System.out.println("world")'
          , '-----'
          , ''
          ].join('\n')
        );
        done();
      } else {
        console.log(stderr);
      }
    });
  });

});
