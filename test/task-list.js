var codedown = require('../lib/codedown.js');
var assert = require('assert');

describe('task list', function(){
  it('should not leak brackets', function (done) {
    var input =
      [
        "* [X] checked",
        "* [ ] unchecked",
        "",
        "```lang",
        "code",
        "```",
      ].join('\n');
    var output = codedown(input, "lang");
    assert.equal(output, 'code');
    done();
  });
});
