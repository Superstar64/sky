# Sky
Lambda Calculus to Ski compiler and runtime.

This is a compiler from lambda calculus expressions into an sk combinater based byte code and a runtime to evaluate it.

This is inspired by [Miranda](https://en.wikipedia.org/wiki/Miranda_(programming_language))'s compilation model and [Lazy K](https://tromp.github.io/cl/lazy-k.html).
## Runtime
The runtime only evaluates and prints output, it does not take input.
The runtime excepts that the input code will be an expression with the type stream given:
```
type bool = forall a. a -> a -> a
type byte = forall r. (bool -> bool -> bool -> bool -> bool -> bool -> bool -> bool -> r) -> r
type list a = forall r. r -> (a -> list a -> r) -> r
type stream = list byte
```
These types use [Mogensen Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding).
Notice that `list` uses a recursive type rather then Boehm-Berarducci encoding.

`1` is encoded as `λx y. x`.

## Format
The byte code format is very similar to [Iota](https://en.wikipedia.org/wiki/Iota_and_Jot) except that it uses `s` and `k` rather then just `i`.
```
code = "0" code code | "1" | "2"
```
Where `1` is `k` and `2` is `s`.

The format the compiler emits in is configurable but format the runtime accepts is not. See ``./ski --help`` for more details.

## Language
* variables : `x`
* lambdas : `a => x`
* application : `f x`
* let-in : `a = b; x`
* parenthesis : `(x)`
* character : `'a'`
* nil : `[]`
* cons : `x : xs`

The parenthesis are equivalent to the inner term.
The let expressions are equivalent to creating a lambda and immediately calling it.
`character`, `nil`, and `cons` use the byte, list and list encoding respectively. 

C-style ``//`` comments are supported

## Building
A Glasgow Haskell compiler, alongside the Megaparsec library, is required to build the compiler.
The Gnu Compiler Collection (for C), Make is required to build the runtime.
If your on debian based distros, you can install `ghc`, `libghc-megaparsec-dev`, `gcc`, and `make`.
Otherwise, install ghc, cabal, gcc, and make though your normal operating system means and run `cabal install megaparsec`.
Run ``make`` to build the executables and ``make samples`` to run the samples.

# Recommended Video
* For a general introduction into the lambda calculus: [Lambda Calculus - Fundamentals of Lambda Calculus & Functional Programming in JavaScript
](https://youtu.be/3VQ382QG-y4).
* For a general introduction into ski combinators and graph reduction: ["An Introduction to Combinator Compilers and Graph Reduction Machines" by David Graunke](https://youtu.be/GawiQQCn3bk).
