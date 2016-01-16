rundown is a little utility to extract code blocks from Markdown files.
It is inspired by [literate Haskell][1], and can be used to validate the
correctness of code embedded in Markdown.

## Quick start

First, install the marked library:

```
$ npm install marked
```

Then, run rundown:

```
$ ./rundown.js 
usage: rundown.js <markdown file> <lang>
ex: rundown.js myfile.md haskell
```

rundown extracts the code blocks designated as language `<lang>`, and
outputs them to stdout.

You can pipe the output of rundown to a language interpreter:

```
$ ./rundown.js README.md haskell | runhaskell
42
```

```
$ ./rundown.js README.md javascript | node
42
```

```
$ ./rundown.js README.md scala | scala
scala> val x = 42
x: Int = 42
scala> println(x)
42
scala> :quit
```

## Examples

This readme is a Markdown file, so we can use rundown to extract code
from it.

In the following code blocks, let's set `x` to 42 in different
languages:

```haskell
x :: Int
x = 42
```

```javascript
var x = 42;
```

```scala
val x = 42
```

Now let's print `x` it to stdout in different languages:

```haskell
main :: IO ()
main = putStrLn $ show x
```

```javascript
console.log(x);
```

```scala
println(x)
```

[1]: https://wiki.haskell.org/Literate_programming
