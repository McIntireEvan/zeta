zeta
====

zeta is a math parser written in Haskell.

It is split up into a lexer, parser, interpreter, and a web interface.

Lexer
-----

The Lexer works by pairing regular expressions with a token that represents that
string. It checks the start of a string against all the regex, and picks the one
that works. There's also a bit more evaluation to clean up unneeded tokens and
to store values in tokens, like with TokInt

Parser
------

The Parser works by defining different levels of precedence for tokens, and then
turning those tokens into nested expressions that can be evaluated.

The precedence levels and code to evaluate them can be seen as a right-recursive
context-free grammar. (This will be expanded on soon).

Interpreter
-----------

The Interpreter simply recursively evalues expressions and their terms into
integers.

Todo
====

There's still a lot of work to be done on this, here are some of the next steps

* Address all the TODOs in the code
* Expand to include parenthesis and exponents
* Decimal support
* Begin work on a web interface