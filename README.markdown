# ITProTV's Haskell style guide

> [Haskell 2010](https://www.haskell.org/onlinereport/haskell2010/) ought to be enough for anybody.

This guide describes how we like to write [Haskell](https://haskell-lang.org) here at [ITProTV](https://itpro.tv).
It focuses more on style and best practices than formatting and layout.
These are recommendations, not hard and fast rules.
Deviating from the guide is allowed, but be prepared to defend your decision in code review.
In general we prefer clarity over cleverness.
Try to write code that is easy to understand, debug, and modify.

## Table of contents

- [Avoid compiler warnings](#avoid-compiler-warnings)
- [Apply HLint suggestions](#apply-hlint-suggestions)
- [Format with Hindent](#format-with-hindent)
- [Prefer `let` over `where`](#prefer-let-over-where)
- [Prefer `do` notation](#prefer-do-notation)
- [Avoid pure `do`](#avoid-pure-do)
- [Avoid writing partial functions](#avoid-writing-partial-functions)
- [Avoid using partial functions](#avoid-using-partial-functions)
- [Avoid throwing exceptions](#avoid-throwing-exceptions)
- [Prefer qualified imports](#prefer-qualified-imports)
- [Prefer unqualified operators](#prefer-unqualified-operators)
- [Prefer functions over operators](#prefer-functions-over-operators)
- [Prefer unique identifiers](#prefer-unique-identifiers)

## Avoid compiler warnings

Code should compile with `-Weverything` and `-Werror`.
We ignore a few warnings:

- `-Wno-implicit-prelude`
- `-Wno-safe`
- `-Wno-unsafe`

## Apply HLint suggestions

We use [HLint](https://github.com/ndmitchell/hlint#readme) to lint all Haskell code.
It suggests many good ways to improve code.
We use a somewhat custom configuration, but the exact details aren't too important.

https://neilmitchell.blogspot.com/2009/09/how-i-use-hlint.html

## Format with Hindent

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

## Prefer `do` notation

`do` notation is generally easier to read because it's similar to imperative code.
It's also easier to modify, for example by adding logging.
Compact expressions using explicit monadic operators like `(>>=)` should be avoided.

``` hs
-- bad
getLine >>= print

-- good
do line <- getLine
   print line
```

## Avoid pure `do`

Normally `do` notation is used for monadic expressions.
It is possible to use it with pure expressions, but it's confusing.

``` hs
-- bad
double x = do
  let two = 2
  x * two

-- good
double x =
  let two = 2
  in x * two
```

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

## Prefer unqualified operators

Qualified operators are visually noisy and can be hard to read.
Import them explicitly instead of using them from a qualified import.

``` hs
-- bad
import qualified Data.Aeson as Aeson
Aeson.object [ "successful" Aeson..= True ]

-- good
import Data.Aeson ((.=))
import qualified Data.Aeson
Aeson.object [ "successful" .= True ]
```

## Prefer functions over operators

When both a function and an operator are available,
prefer the function over the operator.
Functions always follow the same rules,
unlike operators which force you to take precedence into consideration.

``` hs
-- bad
scooby & snack .~ True

-- good
set snack True scooby
```

## Prefer unique identifiers

Any exported identifier within a project should aim to be unique in the context of that project.
This makes it easy to import the entire project and play around in the REPL without excessive qualification.
The redundancy, as in `User.userName user`, is not too big of a problem in practice.

``` hs
-- bad
data User = User { name :: Text }

-- good
data User = User { userName :: Text }
```
