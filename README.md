codedown is a little utility to extract code blocks from Markdown files.

Inspired by [literate Haskell][1], codedown can be used to:

* Validate the correctness of code embedded in Markdown
* Run code embedded in Markdown
* Ship code and Markdown together in harmony

## Quick start

First, install the marked library:

```
$ npm install marked
```

Then, run codedown:

```
$ ./codedown.js 
usage: codedown.js <markdown file> <lang>
ex: codedown.js myfile.md haskell
```

codedown extracts the code blocks designated as language `<lang>`, and
outputs them to stdout.

You can pipe the output of codedown to a language interpreter:

```
$ ./codedown.js README.md haskell | runhaskell
42
```

```
$ ./codedown.js README.md javascript | node
42
```

```
$ ./codedown.js README.md scala | scala
scala> val x = 42
x: Int = 42
scala> println(x)
42
scala> :quit
```

## Examples

This readme is a Markdown file, so we can use codedown to extract code
from it.

In the following code blocks, let's set `x` to 42 in different
languages:

*Haskell:*

```haskell
x :: Int
x = 42
```

*JavaScript:*

```javascript
var x = 42;
```

*Scala:*

```scala
val x = 42
```

Now let's print `x` it to stdout in different languages:

*Haskell:*

```haskell
main :: IO ()
main = putStrLn $ show x
```

*JavaScript:*

```javascript
console.log(x);
```

*Scala:*

```scala
println(x)
```

[1]: https://wiki.haskell.org/Literate_programming
