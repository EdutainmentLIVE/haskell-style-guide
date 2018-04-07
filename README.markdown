# ITProTV's Haskell style guide

This guide describes how we like to write [Haskell](https://haskell-lang.org) here at [ITProTV](https://itpro.tv).
It focuses more on style and best practices than formatting and layout.
These are recommendations, not hard and fast rules.
Deviating from the guide is allowed, but be prepared to defend your decision in code review.

In general we prefer clarity over cleverness.
Try to write code that is easy to understand, debug, and modify.

## Table of contents

- [Formatting](#formatting)
- [Prefer `let` over `where`](#prefer-let-over-where)

## Formatting

We use [`hindent-5.2.5`](https://hackage.haskell.org/package/hindent-5.2.5) to format all Haskell code with this config:

``` yaml
# .hindent.yaml
force-trailing-newline: true
indent-size: 2
line-length: 79
sort-imports: true
```

https://chrisdone.com/posts/hindent-5

## Prefer `let` over `where`

`let` is an expression and follows the same rules as everything else.
`where` can only be used with declarations and can be awkward to use with `do` notation, guards, and point-free code.

``` hs
-- bad
3 * kibi where kibi = 2 ^ 10

-- good
let kibi = 2 ^ 10 in 3 * kibi
```

https://stackoverflow.com/questions/4362328/haskell-where-vs-let
