# ITProTV's Haskell style guide

This guide describes how we like to write [Haskell](https://haskell-lang.org) here at [ITProTV](https://itpro.tv).
It focuses more on style and best practices than formatting and layout.

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

## Prefer `let` over `where`

``` hs
-- bad
3 * kibi where kibi = 2 ^ 10

-- good
let kibi = 2 ^ 10 in 3 * kibi
```
