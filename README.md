# should-not-typecheck [![Build Status](https://travis-ci.org/CRogers/should-not-typecheck.svg?branch=master)](https://travis-ci.org/CRogers/should-not-typecheck)

`should-not-typecheck` is a Haskell library which allows you to assert that an expression does not typecheck in your unit tests. It provides one function, `shouldNotTypecheck :: a -> Assertion`, which takes an expression and will fail the test if it typechecks. `shouldNotTypecheck` returns an HUnit `Assertion` (so it can be used with both `HUnit` and `hspec`).

## Example (hspec)

The secret sauce is the [Deferred Type Errors GHC extension](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/defer-type-errors.html). This allows you to write a non-typechecking expression which will throw an exception at run time (rather than erroring out at compile time). `shouldNotTypecheck` tries to catch that exception and fails the test if no deferred type error is caught.

```haskell
{-# OPTIONS_GHC -fdefer-type-errors #-} -- Very important!

module Main where

import Test.Hspec (hspec, describe, it)
import Test.ShouldNotTypecheck (shouldNotTypecheck)

main :: IO ()
main = hspec $ do
  describe "Type Tests" $ do
    it "should not allow an Int to be a String" $
      shouldNotTypecheck (4 :: String)
```

It can be used similarly with HUnit.

## Motivation

Sometimes you want to test that it is impossible to type a particular expression.

```haskell
{-# LANGUAGE GADTs #-}
data Expr t where
  IntVal :: Int -> Expr Int
  BoolVal :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  -- ...
```

We might want to make sure that `Add (BoolVal True) (IntVal 4)` is not well typed. However, we can't even compile code like this in a unit test! Hence `should-not-typecheck`.

## Limitations

Unfortunately, we have to turn on deferred type errors for the entire test file rather than just specific expressions. This means that any type error will compile but fail at runtime. For example:

```haskell
{-# OPTIONS_GHC -fdefer-type-errors #-}

-- ...

main = hspec $ do
  decsribe 4 $ do
   -- ...
```

Will create a warning at compile time but not an error. All of the ill-typed expressions will also produce warnings and it will hard to quickly see which ones matter. The upside is that the test-suite will still fail if there are errors.

### Workaround

You can separate out the ill-typed expressions and test boilerplate into separate classes and only turn on deferred type errors the expressions. This means that type errors in test code will still be found at compile time. The downside is your tests may now be harder to read.
