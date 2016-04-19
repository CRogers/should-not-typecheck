{-# LANGUAGE CPP, RankNTypes, GADTs #-}
#if __GLASGOW_HASKELL__ >= 800
#define TheExc TypeError
#else
#define TheExc ErrorCall
#endif
module Test.ShouldNotTypecheck (shouldNotTypecheck) where

import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate, try, throwIO, TheExc(..))
import Data.List (isSuffixOf)
import Test.HUnit.Lang (Assertion, assertFailure)

-- Taken from base-4.8.0.0:Data.List
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf []    _                    = True
isSubsequenceOf _     []                   = False
isSubsequenceOf a@(x:a') (y:b) | x == y    = isSubsequenceOf a' b
                               | otherwise = isSubsequenceOf a b

{-|
  Takes one argument, an expression that should not typecheck.
  It will fail the test if the expression does typecheck.
  Requires Deferred Type Errors to be enabled for the file it is called in.
  See the <https://github.com/CRogers/should-not-typecheck#should-not-typecheck- README>
  for examples and more infomation.
-}
#if __GLASGOW_HASKELL__ >= 800
shouldNotTypecheck :: NFData a => (() ~ () => a) -> Assertion
#else
shouldNotTypecheck :: NFData a => a -> Assertion
#endif
-- The type for GHC-8.0.1 is a hack, see https://github.com/CRogers/should-not-typecheck/pull/6#issuecomment-211520177
shouldNotTypecheck a = do
  result <- try (evaluate $ force a)
  case result of
    Right _ -> assertFailure "Expected expression to not compile but it did compile"
    Left e@(TheExc msg) -> case isSuffixOf "(deferred type error)" msg of
      True -> case isSubsequenceOf "No instance for" msg && isSubsequenceOf "NFData" msg of
        True -> assertFailure $ "Make sure the expression has an NFData instance! See docs at https://github.com/CRogers/should-not-typecheck#nfdata-a-constraint. Full error:\n" ++ msg
        False -> return ()
      False -> throwIO e
