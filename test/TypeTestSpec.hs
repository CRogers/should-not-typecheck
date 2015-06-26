{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

import Control.Exception
import Test.Hspec
import Test.Hspec.Expectations (expectationFailure)
import Test.HUnit.Lang (performTestCase)
import TypeTest

pattern TestCaseSuccess = Nothing
pattern TestCaseFailure msg = Just (True, msg)
pattern TestCaseError msg = Just (False, msg)

shouldFailAssertion :: IO () -> IO ()
shouldFailAssertion value = do
  result <- performTestCase value
  case result of
    Just (True, _) -> return ()
    Just (False, msg) -> expectationFailure $ "Raised an error " ++ msg
    Nothing -> expectationFailure "Did not throw an assertion error"

shouldThrowException :: Exception e => e -> IO () -> IO ()
shouldThrowException exception value = do
  result <- performTestCase value
  case result of
    TestCaseSuccess -> expectationFailure "Test case succeeded, did not throw exception"
    TestCaseFailure msg -> expectationFailure "Test case failed, did not throw exception"
    TestCaseError msg -> case msg == show exception of
      True -> return ()
      False -> expectationFailure "Incorrect exception propagated"

main :: IO ()
main = hspec $ do
  describe "Type Test" $ do
    it "should not throw an assertion error when an expression is ill typed" $ do
      shouldNotCompile ("foo" :: Int)

    it "should throw an assertion error when an expression is well typed" $ do
      shouldFailAssertion (shouldNotCompile ("foo" :: String))

    it "should throw an actual exception and not fail the assertion if the expression contains an non-HUnitFailure exception" $ do
      let exception = NoMethodError "lol"
      shouldThrowException exception (shouldNotCompile (throw exception))
