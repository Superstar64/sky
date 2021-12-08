# LambdaSki
Simple Lambda Calculus to Ski compiler.

This is a simple program that converts lambda calculus expressions into sk combinator expressions.

The Haskell Platform and Megaparsec are required to build.

# Language
The language has 5 types of terms:
* variables : ``x``
* lambdas : ``a => x``
* application : ``f x``
* let-in : ``a = b; x``
* parenthesis : ``(x)``

The parenthesis are equivalent to the inner term. The let expressions are equivalent to creating a lambda and immediately calling it.

C-style ``//`` comments are supported

# Building
The Haskell Platform and Megaparsec are required to build.
Run ``make`` to build the executable and ``make samples`` to build all the samples.
