## RuSKI

Ruski is a simple, zero-dependency implementation of combinator calculus in Rust.
It is based on the crate [lambda_calculus](https://github.com/ljedrz/lambda_calculus) and
adds elements from [Wolfram](https://writings.stephenwolfram.com/2020/12/combinators-a-centennial-view/)


### Features

* Parser for SKI expressions
* β reduction of SKI expressions (currently: leftmost outermost, leftmost innermost)
* Tree representation of expressions with shared subexpressions.
* Graph representation of reductions with shard reductions paths (draft)

