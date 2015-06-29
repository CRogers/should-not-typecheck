# should-not-typecheck [![Build Status](https://travis-ci.org/CRogers/should-not-typecheck.svg?branch=master)](https://travis-ci.org/CRogers/should-not-typecheck) [![Hackage](https://img.shields.io/hackage/v/should-not-typecheck.svg)](https://hackage.haskell.org/package/should-not-typecheck)

`should-not-typecheck` is a Haskell library which allows you to assert that an expression does not typecheck in your tests. It provides one function, `shouldNotTypecheck`, which takes an expression and will fail the test if it typechecks. `shouldNotTypecheck` returns an HUnit `Assertion` (so it can be used with both `HUnit` and `hspec`).

## Example (hspec)

The secret sauce is the [Deferred Type Errors GHC extension](https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/defer-type-errors.html). This allows you to write a ill-typed expression which will throw an exception at run time (rather than erroring out at compile time). `shouldNotTypecheck` tries to catch that exception and fails the test if no deferred type error is caught.

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

### `NFData a` constraint

Haskell is a lazy language - deferred type errors will not get evaluated unless we explicitly and deeply force (evaluate) the value. [`NFData`](https://hackage.haskell.org/package/deepseq-1.4.1.1/docs/Control-DeepSeq.html#t:NFData) is a typeclass from the [`deepseq`](https://hackage.haskell.org/package/deepseq) library which allows you to describe how to fully evaluate an expression (convert it to Normal Form). `shouldNotTypecheck` uses this typeclass to fully evaluate expressions passed to it. For vanilla Haskell types you only need to derive `Generic` and the `deepseq` class will handle it for you:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)

data SomeType a = WithSome | DataConstructors a
  deriving Generic

instance NFData a => NFData (SomeType a)
```

In GHC 7.10 [`DeriveAnyClass` can be used](https://hackage.haskell.org/package/deepseq-1.4.1.1/docs/Control-DeepSeq.html#v:rnf) to make it even more succinct.

With `deepseq >= 1.4`, this autoderiving `Generic` option is included with the library. With `deepseq <= 1.3` you'll have to use the [`deepseq-generics`](https://hackage.haskell.org/package/deepseq-generics) library as well.

#### GADTs

With more complex datatypes, like GADTs and those existentially quantified, `DeriveGeneric` does not work. You will need to provide an instance for `NFData` yourself, but not to worry as it follows a pattern:

```haskell
{-# LANGUAGE GADTs #-}

import Control.DeepSeq (NFData)

data Expr t where
  IntVal :: Int -> Expr Int
  BoolVal :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int

instance NFData (Expr t) where
  rnf expr = case expr of
    IntVal i  -> rnf i -- call rnf on every subvalue
    BoolVal b -> rnf b
    Add l r   -> rnf l `seq` rnf r -- and `seq` multiple values together

-- Now we can test expressions like:
badExpr = Add (IntVal 4) (BoolVal True)
-- do not typecheck!
```

If you forget to specify provide an `NFData` instance for a type `should-not-typecheck` should warn you.

## Motivation

Sometimes you want to ensure that it is impossible to type a particular expression. For example, imagine if we were making a typesafe Abstract Syntax Tree of mathematical expressions:

```haskell
{-# LANGUAGE GADTs #-}

data Expr t where
  IntVal :: Int -> Expr Int
  BoolVal :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  -- ...
```

We might want to make sure that `Add (BoolVal True) (IntVal 4)` is not well typed. However, we can't even compile code like this to put in a unit test! This is where `should-not-typecheck` steps in.

## Limitations

Unfortunately, we can only turn on deferred type errors for the entire test file rather than just specific expressions. This means that any type error will compile but fail at runtime. For example:

```haskell
{-# OPTIONS_GHC -fdefer-type-errors #-}

-- ...

main :: IO ()
main = hspec $ do
  describe 4 $ do -- Oops!
   -- ...
```

Will create a warning at compile time but not an error. All of the ill-typed expressions we are testing will also produce warnings and it will hard to immediately see which ones matter. The upside is that the test-suite will still fail if there are errors.

### Workaround

You can separate out the ill-typed expressions we are testing and test boilerplate into separate files and only turn on deferred type errors for the expressions. This means that type errors in test code will still be found at compile time. The downside is your tests may now be harder to read.
