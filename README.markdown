# ITProTV's Haskell style guide

This guide describes how we like to write [Haskell](https://haskell-lang.org) here at [ITProTV](https://itpro.tv).
It focuses more on style and best practices than formatting and layout.
We use [`hindent-5.2.5`](https://hackage.haskell.org/package/hindent-5.2.5) to format all Haskell code with this config:

``` yaml
# .hindent.yaml
force-trailing-newline: true
indent-size: 2
line-length: 79
sort-imports: true
```
## Use exceptions only for exceptional circumstances not for logic
Exceptions should be reserved for truly exceptional situations rather than programming logic.
