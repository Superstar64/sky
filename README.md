# LambdaSki
Simple Lambda Calculus to Ski compiler.

This is a simple program that converts lambda calculus expressions into sk combinator expressions.


The Haskell Platform and Megaparsec are required to build.

# Output Format
An optional argument is taken to deteremine the output format, where 'f' deteremines where to put the caller term and 'x' deteremines where to put the argument term.
Examples:
* ``f(x)`` emits outputs like ``s(k)(k)``
* ``afx`` emits outputs like ``aaskk``

The terms 's' and 'k' can also be specified in the second and third argument
Examples:
* ``f(k) a b`` emits output like ``a(b)(b)``

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
Run either ``make`` to build the executable.
