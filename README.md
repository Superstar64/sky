# Sky
Lambda Calculus to SKI compiler and runtime.

This project is composed of:

* A Haskell compiler from lambda calculus expressions to a custom ski byte code
* A C interpeter that runs the ski expressions via graph reduction
* A Python interpeter that runs the ski expressions via compilation to lazy thunks.

This is inspired by [Miranda](https://en.wikipedia.org/wiki/Miranda_(programming_language))'s compilation model and [Lazy K](https://tromp.github.io/cl/lazy-k.html).

## Runtime
The runtime only evaluates and prints output, it does not take input. It excepts that the input code will be a stream of bytes with the given type encodings:
```
type bool = forall a. a -> a -> a
type byte = forall r. (bool -> bool -> bool -> bool -> bool -> bool -> bool -> bool -> r) -> r
type list a = forall r. r -> (a -> list a -> r) -> r
type stream = list byte
```
These types use [Mogensen Scott encoding](https://en.wikipedia.org/wiki/Mogensen%E2%80%93Scott_encoding).
Notice that `list` uses a recursive type rather then Boehm-Berarducci encoding.

A `1` bit is encoded as true (`λx y. x`).

## Language
* variables : `x`
* lambdas : `x => e`
* application : `e e'`
* let-in : `x = e; e'`
* parenthesis : `(e)`
* character : `'a'`
* nil : `[]`
* cons : `e : e'`
* axiom : `_builtin x` `_builtin 'x'`

The parenthesis are equivalent to the inner term.
The let expressions are equivalent to creating a lambda and immediately calling it.
`character`, `nil`, and `cons` use the byte, list and list encoding respectively.
Axiom are emitted direct into the output.

C-style ``//`` comments are supported

## Format
The byte code format is very similar to [Iota](https://en.wikipedia.org/wiki/Iota_and_Jot) except that it uses `s` and `k` rather then just `i`.
```
code = "0" code code | "1" | "2"
```

Where `0 e e'` is `e(e')`, `1` is `k`, and `2` is `s`.

The C interpeter has some internal additional axioms to used by `runtime.lambda` to aid graph reduction.

The format the compiler emits in is configurable but format the runtime accepts is not. See ``./sky --help`` for format configuration details.

## Building
### Debian
Install these packages
* make
* gcc
* python3
* ghc
* libghc-megaparsec-dev

### Generic
Ensure that the Gnu C Compiler, Python, the Glasgow Haskell Compiler, Cabel are all installed, then run:
```
cabal install megaparsec --lib
```

Run ``make`` to build the executables and ``make samples`` to run the samples.

# Recommended Videos
* For a general introduction into the lambda calculus: [Lambda Calculus - Fundamentals of Lambda Calculus & Functional Programming in JavaScript
](https://youtu.be/3VQ382QG-y4).
* For a general introduction into ski combinators and graph reduction: ["An Introduction to Combinator Compilers and Graph Reduction Machines" by David Graunke](https://youtu.be/GawiQQCn3bk).
