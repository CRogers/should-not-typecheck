# should-not-typecheck [![Build Status](https://travis-ci.org/CRogers/should-not-typecheck.svg?branch=master)](https://travis-ci.org/CRogers/should-not-typecheck)

`should-not-typecheck` is a Haskell library which allows you to assert that an expression does not typecheck in your unit tests. It provides one function, `shouldNotTypecheck :: a -> Assertion`, which takes an expression and returns an HUnit assertion (so it can be used in `HUnit` and `hspec`).

## Example (hspec)

The secret sauce is the [Deferred Type Errors GHC extension](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/defer-type-errors.html). This allows you to write a non-typechecking expression which will throw an exception at run time (rather than erroring out at compile time). This library trys to catch that error and fails the test if no deferred type error is caught.

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
