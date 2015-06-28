module Test.ShouldNotTypecheck (shouldNotTypecheck) where

import Control.DeepSeq (force, NFData)
import Control.Exception (evaluate, try, throwIO, ErrorCall(..))
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
shouldNotTypecheck :: NFData a => a -> Assertion
shouldNotTypecheck a = do
  result <- try (evaluate $ force a)
  case result of
    Right _ -> assertFailure "Expected expression to not compile but it did compile"
    Left (ErrorCall msg) -> case isSuffixOf "(deferred type error)" msg of
      True -> case isSubsequenceOf "No instance for" msg && isSubsequenceOf "NFData" msg of
        True -> assertFailure $ "Make sure the expression has an NFData instance! See docs at <todo>. Full error:\n" ++ msg
        False -> return ()
      False -> throwIO (ErrorCall msg)
