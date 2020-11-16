# LambdaSki
Simple Lambda Calculus to Ski compiler.

This is a simple program that converts lambda calculus expressions into sk combinator expressions.


The Haskell Platform and Megaparsec are required to build.

# Output Format
An optional argument is taken to deteremine the output format, where 'f' deteremines where to put the caller term and 'x' deteremines where to put the argument term.
Examples:
* ``f(x)`` emits outputs like ``s(k)(k)``
* ``afx`` emits outputs like ``aaskk``

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

# Building
The Haskell Platform and Megaparsec are required to build.
Run either ``make`` or ``cabal configure && cabal build`` to build the executable.
