module Test.ShouldNotTypecheck (shouldNotTypecheck) where

import Control.Exception (evaluate, try, throwIO, ErrorCall(..))
import Data.List (isSuffixOf)
import Test.HUnit.Lang (Assertion, assertFailure)

{-|
  Takes one argument, an expression that should not typecheck.
  It will fail the test if the expression does typecheck.
  Requires Deferred Type Errors to be enabled for the file it is called in.
  See the <https://github.com/CRogers/should-not-typecheck#should-not-typecheck- README>
  for examples and more infomation.
-}
shouldNotTypecheck :: a -> Assertion
shouldNotTypecheck a = do
  result <- try (evaluate a)
  case result of
    Right _ -> assertFailure "Expected expression to not compile but it did compile"
    Left (ErrorCall msg) -> case isSuffixOf "(deferred type error)" msg of
      True -> return ()
      False -> throwIO (ErrorCall msg)
