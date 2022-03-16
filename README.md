# Rust s-expression Parser

Implements an s-expression parser for https://rosettacode.org/wiki/S-expressions in Rust.

It's a zero copy implementation. That means the tokens are implemented as slices of the input string.

The Token Parser (lexer) is implemented as an iterator and the expression parser is implemented as a second iterator that calls the token iterator. So it also does everything in one pass.


