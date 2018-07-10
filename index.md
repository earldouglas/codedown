<script type="text/javascript" src="https://cdn.jsdelivr.net/npm/marked/lib/marked.js"></script>
<script type="text/javascript" src="https://cdn.jsdelivr.net/npm/codedown/lib/codedown.js"></script>
<script type="text/javascript">
  function demo() {
    var src = document.getElementById('src').value;
    var lang = document.getElementById('lang').value;
    document.getElementById('output').innerHTML = codedown(src, lang);
    return false;
  }
</script>

<style type="text/css">
  .table {
    display: table;
    width: 100%;
  }
  .table-row {
    display: table-row;
  }
  .table-left {
    display: table-cell;
    vertical-align: top;
    text-align: right;
    padding-right: 1ex;
    padding-bottom: 0.5em;
    width: 1px;
  }
  .table-right {
    display: table-cell;
    padding-bottom: 0.5em;
    vertical-align: top;
  }
  textarea, input#lang {
    font-family: monospace;
    padding: 0.25em 0.75ex;
  }
  textarea {
    width: 72ex;
    height: 36em;
  }
  pre#output {
    border-left: 2px solid #ccc;
    margin-left: 1ex;
    padding-left: 2ex;
  }
</style>

This is a simple in-browser demo of
[Codedown](https://github.com/earldouglas/codedown).

Type some Markdown in the `textarea` below, choose a language to
extract, and hit *[Extract]* to run it through Codedown.

<form onsubmit="return demo();">
 <div class="table">

  <div class="table-row">
   <div class="table-left">Source:</div>
   <div class="table-right"><textarea class="left" id="src"></textarea></div>
  </div>

  <div class="table-row">
   <div class="table-left">Language:</div>
   <div class="table-right"><input id="lang" value="javascript"/></div>
  </div>

  <div class="table-row">
   <div class="table-left"></div>
   <div class="table-right"><input type="submit" value="Extract" /></div>
  </div>

  <div class="table-row">
   <div class="table-left">Output:</div>
   <div class="table-right"><pre id="output"></pre></div>
  </div>

 </div>
</form>

<script type="text/javascript">
  document.getElementById('src').value =
    [ '# This is Markdown!'
    , ''
    , '## About'
    , ''
    , 'This is a simple Markdown document to demonstrate code extraction using'
    , 'codedown.'
    , ''
    , 'Here\'s a JavaScript code block:'
    , ''
    , '```javascript'
    , 'var x = 42;'
    , '```'
    , ''
    , '## Usage'
    , ''
    , 'Send this Markdown content through codedown by posting it to codedown.'
    , ''
    , 'Here\'s another JavaScript code block:'
    , ''
    , '```javascript'
    , 'console.log(x);'
    , '```'
    ].join('\n');
</script>
