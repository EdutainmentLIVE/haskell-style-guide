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
- [Avoid writing partial functions](#avoid-writing-partial-functions)
- [Avoid using partial functions](#avoid-using-partial-functions)
- [Avoid throwing exceptions](#avoid-throwing-exceptions)
- [Prefer qualified imports](#prefer-qualified-imports)

## Formatting

We use [Hindent](https://github.com/commercialhaskell/hindent) to format all Haskell code.
It may not format everything perfectly, but we prefer it to arguing about layout.

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

## Avoid writing partial functions

Partial functions are difficult to work with because they can fail but their type signatures don't express that.
Avoid non-exhaustive pattern matching, `undefined`, and `error`.
Instead prefer types like `Maybe result` or `Either failure success`.

``` hs
-- bad
first (x : _) = x

-- good
first xs = case xs of
  x : _ -> Just x
  _ -> Nothing
```

https://www.parsonsmatt.org/2017/10/11/type_safety_back_and_forth.html

## Avoid using partial functions

Many libraries, including the standard library, come with partial functions that throw exceptions at runtime.
Wherever possible, avoid using these partial functions and prefer their total (non-partial) versions instead.

``` hs
-- bad
head []

-- good
listToMaybe []
```

https://begriffs.com/posts/2013-08-18-dont-be-partial-to-partial-functions.html

## Avoid throwing exceptions

Even if you're already in `IO`, you should prefer expressing failure with types over using `throw`.
Just because `IO` can throw exceptions doesn't mean that the only failure mode in `IO` should be exceptions.

``` hs
-- bad
if canAccess thing user
  then pure thing
  else throw (userError "cannot access thing")

-- good
if canAccess thing user
  then pure (Just thing)
  else pure Nothing
```

https://np.reddit.com/r/haskell/comments/5bkqf1/exceptions_best_practices_in_haskell/

## Prefer qualified imports

Qualifying imports makes it clear where things come from at the cost of making things more verbose.

``` hs
-- bad
import Data.Aeson
import Data.Text (Text, pack, unpack)

-- good
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
```

When importing a module named `A.B.….Z`, generally prefer aliasing it as `Z`.
If that doesn't feel right, please consult the following table:

Module | Alias
--- | ---
`Foreign` | `Foreign`
`Data.List` | `List`
`Data.List.NonEmpty` | `NonEmpty`
`Control.Monad.IO.Class` | `IO`
`Data.ByteString` | `Bytes`
`Data.ByteString.Lazy` | `LazyBytes`

https://www.parsonsmatt.org/2017/06/23/on_naming_things.html
