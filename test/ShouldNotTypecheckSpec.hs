{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

import Control.Exception
import Test.Hspec
import Test.Hspec.Expectations (expectationFailure)
import Test.HUnit.Lang (performTestCase)
import Test.ShouldNotTypecheck

pattern TestSuccess = Nothing
pattern TestFailure msg = Just (True, msg)
pattern TestError msg = Just (False, msg)

shouldFailAssertion :: IO () -> IO ()
shouldFailAssertion value = do
  result <- performTestCase value
  case result of
    TestSuccess -> expectationFailure "Did not throw an assertion error"
    TestFailure _ -> return ()
    TestError msg -> expectationFailure $ "Raised an error " ++ msg

shouldThrowException :: Exception e => e -> IO () -> IO ()
shouldThrowException exception value = do
  result <- performTestCase value
  case result of
    TestSuccess -> expectationFailure "Did not throw exception: assertion succeeded"
    TestFailure msg -> expectationFailure "Did not throw exception: assertion failed"
    TestError msg -> case msg == show exception of
      True -> return ()
      False -> expectationFailure "Incorrect exception propagated"

main :: IO ()
main = hspec $ do
  describe "shouldNotCompile" $ do
    it "should not throw an assertion error when an expression is ill typed" $ do
      shouldNotTypecheck ("foo" :: Int)

    it "should throw an assertion error when an expression is well typed" $ do
      shouldFailAssertion (shouldNotTypecheck ("foo" :: String))

    it "should throw an actual exception and not fail the assertion if the expression contains an non-HUnitFailure exception" $ do
      let exception = NoMethodError "lol"
      shouldThrowException exception (shouldNotTypecheck (throw exception))

    it "should propagate an actual exception and not fail the assertion if the expression contains a non-deferred ErrorCall exception" $ do
      let exception = ErrorCall "yay"
      shouldThrowException exception (shouldNotTypecheck (throw exception))
