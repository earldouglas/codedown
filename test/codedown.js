var assert  = require('assert');
var process = require('child_process');

describe('codedown', function(){

  it('should require a <lang> argument', function (done) {
    process.exec('./codedown.js', function (err, stdout, stderr) {
      if (!err) {
        assert.equal(
          [ 'Usage: codedown <lang> [...]'
          , ''
          , 'Options:'
          , '--separator <separator line>'
          , '--section <section number>'
          , ''
          , 'Example:'
          , 'cat README.md | codedown haskell --separator=----- --section 1.3'
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
    process.exec('cat README.md | ./codedown.js haskell --separator=-----', function (err, stdout, stderr) {
      if (!err) {
        assert.equal(
          stdout,
          [ 'x :: Int'
          , 'x = 42'
          , '-----'
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

  it('should extract code by section number (1)', function (done) {
    process.exec('cat README.md | ./codedown.js haskell --section 1', function (err, stdout, stderr) {
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

  it('should extract code by section number (1.3)', function (done) {
    process.exec('cat README.md | ./codedown.js haskell --section 1.3', function (err, stdout, stderr) {
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

  it('should extract code by section number (1.3.1)', function (done) {
    process.exec('cat README.md | ./codedown.js haskell --section 1.3.1', function (err, stdout, stderr) {
      if (!err) {
        assert.equal(
          stdout,
          [ 'x :: Int'
          , 'x = 42'
          , ''
          ].join('\n')
        );
        done();
      } else {
        console.log(stderr);
      }
    });
  });

  it('should extract code by section number (1.3.2)', function (done) {
    process.exec('cat README.md | ./codedown.js haskell --section 1.3.2', function (err, stdout, stderr) {
      if (!err) {
        assert.equal(
          stdout,
          [ 'main :: IO ()'
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

  it('should extract code by section name (## Examples)', function (done) {
    process.exec('cat README.md | ./codedown.js haskell --section "## Examples"', function (err, stdout, stderr) {
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

  it('should extract code by section name (### Variables in different languages)', function (done) {
    process.exec('cat README.md | ./codedown.js haskell --section "### Variables in different languages"', function (err, stdout, stderr) {
      if (!err) {
        assert.equal(
          stdout,
          [ 'x :: Int'
          , 'x = 42'
          , ''
          ].join('\n')
        );
        done();
      } else {
        console.log(stderr);
      }
    });
  });

  it('should extract code by section name (### Console output in different languages)', function (done) {
    process.exec('cat README.md | ./codedown.js haskell --section "### Console output in different languages"', function (err, stdout, stderr) {
      if (!err) {
        assert.equal(
          stdout,
          [ 'main :: IO ()'
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

  it('should extract any code if language is *', function (done) {
    process.exec('cat README.md | ./codedown.js "*"', function (err, stdout, stderr) {
      if (!err) {
        assert.equal(
          stdout,
          [ 'x :: Int'
          , 'x = 42'
          , ''
          , 'var x = 42;'
          , ''
          , 'val x = 42'
          , ''
          , 'main :: IO ()'
          , 'main = putStrLn $ show x'
          , ''
          , 'console.log(x);'
          , ''
          , 'println(x)'
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
