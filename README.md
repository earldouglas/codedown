[![Build Status][build-badge]][build-link]
[![Coverage Status][coverage-badge]][coverage-link]
[![npm version][release-badge]][release-link]

[build-badge]: https://github.com/earldouglas/codedown/workflows/build/badge.svg
[build-link]: https://github.com/earldouglas/codedown/actions
[coverage-badge]: https://coveralls.io/repos/github/earldouglas/codedown/badge.svg
[coverage-link]: https://coveralls.io/github/earldouglas/codedown
[release-badge]: https://badge.fury.io/js/codedown.svg
[release-link]: https://www.npmjs.com/package/codedown

# Codedown

Codedown is a little utility to extract code blocks from Markdown files.
Inspired by [literate
Haskell](https://wiki.haskell.org/Literate_programming), Codedown can be
used to:

* Validate the correctness of code embedded in Markdown
* Run code embedded in Markdown
* Ship code and Markdown together in harmony

![](codedown.gif)

## Quicker start

To skip installing Codedown locally, [try it
online](https://earldouglas.github.io/codedown/).

## Quick start

Install Codedown:

```
$ npm install -g codedown
```

Run Codedown:

```
Usage: codedown <lang> [...]

Options:
--separator <separator line>
--section <section number>

Example:
cat README.md | codedown haskell --separator=----- --section 1.3
```

Codedown reads Markdown from stin, extracts the code blocks designated
as language `<lang>`, and outputs them to stdout.  The example above
extracts the Haskell code from section 1.3 of this file, and outputs it
with five dashes in between each block:

```
x :: Int
x = 42
-----
main :: IO ()
main = putStrLn $ show x
```

We can pipe the output of Codedown to a language interpreter:

```
$ cat README.md | codedown haskell | runhaskell
42
```

```
$ cat README.md | codedown javascript | node
42
```

```
$ cat README.md | codedown scala | xargs -0 scala -e
42
```

## Examples

This readme is a Markdown file, so we can use Codedown to extract code
from it.

### Variables in different languages

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

### Console output in different languages

Now let's print `x` it to stdout in different languages.  This time, the
code blocks are nested within an unordered list:

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

### Docker

Build and run a Docker image:

```
docker build -t codedown:dev .
```

Use it to extract `haskell` code blocks and save to `output.hs`:

```
cat README.md | docker run -i codedown:dev haskell > output.hs
```

## Sections and subsections

The section above is 1.3, counting by headings.  It has two subsections
(1.3.1 and 1.3.2).  We can specify a section number to extract the
content from just that section:

```
$ cat README.md | codedown haskell --section 1.3
x :: Int
x = 42

main :: IO ()
main = putStrLn $ show x
```

```
$ cat README.md | codedown haskell --section 1.3.1
x :: Int
x = 42
```

```
$ cat README.md | codedown haskell --section 1.3.2
main :: IO ()
main = putStrLn $ show x
```

We can also specify a section by heading:

```
cat README.md | ./codedown.js haskell --section '### Variables in different languages'
x :: Int
x = 42
```

## Wildcard matching

Codedown can use wildcards to match file paths, which are used by some
markdown implementations:

*lib/codedown.js:*

```lib/codedown.js
var x = 42;
```

```
$ cat README.md | codedown '**/*.js'
var x = 42
```

Additionally, you can use a special `*` character in place of the language
option to extract any/all code blocks agnostic of language:

```
$ cat README.md | codedown '*'
```

## Separator

If there are multiple code blocks in the same file, we can specify a
separator to insert in between them:

```
$ cat README.md | codedown haskell --separator=-----
x :: Int
x = 42
-----
main :: IO ()
main = putStrLn $ show x
```
