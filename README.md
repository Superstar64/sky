# LambdaSki
Simple Lambda Calculus to Ski Translator.

This is a simple program that converts lambda calculus expressions into sk combinator expressions.

The Haskell Platform and Megaparsec are required to build.

# Language
The language has 5 types of terms:
* variables : ``x``
* lambdas : ``a => x``
* multi-argument lambdas: ``\a b c -> x``
* application : ``f x``
* parenthesis : ``(x)``
* let-in : ``let a = b; x``

The parenthesis are equivalent to the inner term. The let expressions are equivalent to creating a lambda and immediately calling it.

The samples are run through the C preprocessor to remove comments.
