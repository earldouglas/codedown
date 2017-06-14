[![Build status](https://travis-ci.org/earldouglas/codedown.svg?branch=master)](https://travis-ci.org/earldouglas/codedown)
[![Coverage status](https://coveralls.io/repos/github/earldouglas/codedown/badge.svg?branch=master)](https://coveralls.io/github/earldouglas/codedown?branch=master)

Codedown is a little utility to extract code blocks from Markdown files.

Inspired by [literate Haskell][1], codedown can be used to:

* Validate the correctness of code embedded in Markdown
* Run code embedded in Markdown
* Ship code and Markdown together in harmony

![](codedown.gif)

## Quicker start

To skip installing codedown locally, [try it online][2].

## Quick start

Install codedown:

```
$ npm install -g codedown
```

Run codedown:

```
$ codedown
usage: codedown <lang>
ex: codedown haskell
```

Codedown reads Markdown from stin, extracts the code blocks designated
as language `<lang>`, and outputs them to stdout.

You can pipe the output of codedown to a language interpreter:

```
$ cat README.md | codedown haskell | runhaskell
42
```

```
$ cat README.md | codedown javascript | node
42
```

```
$ cat README.md | codedown scala | scala
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

Now let's print `x` it to stdout in different languages.

This time, the code blocks are nested within an unordered list:

* *Haskell:*

  ```haskell
  main :: IO ()
  main = putStrLn $ show x
  ```

* *JavaScript:*

  ```javascript
  console.log(x);
  ```

* *Scala:*

  ```scala
  println(x)
  ```

## Wildcard matching

Codedown can use wildcards to match file paths, which are used by some markdown implementations:

* *lib/codedown.js*

```lib/codedown.js
var x = 42;
```

```
$ cat README.md | codedown '**/*.js'
var x = 42
```

## Separator

If there are multiple code blocks in the same file, you can specify a separator as the third argument:

```java
System.out.println("hello")
```

```java
System.out.println("world")
```

```
$ cat README.md | codedown java -----
System.out.println("hello")
-----
System.out.println("world")
-----
```

[1]: https://wiki.haskell.org/Literate_programming
[2]: https://earldouglas.com/posts/codedown.html
