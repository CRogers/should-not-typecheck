{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

import Control.Exception
import Test.Hspec
import Test.Hspec.Expectations (expectationFailure)
import Test.HUnit.Lang (performTestCase)
import TypeTest

shouldFailAssertion :: IO () -> IO ()
shouldFailAssertion value = do
  result <- performTestCase value
  case result of
    Just (True, _) -> return ()
    Just (False, msg) -> expectationFailure $ "Raised an error " ++ msg
    Nothing -> expectationFailure "Did not throw an assertion error"

main :: IO ()
main = hspec $ do
  describe "Type Test" $ do
    it "should not throw an assertion error when an expression is ill typed" $ do
      shouldNotCompile ("foo" :: Int)

    it "should throw an assertion error when an expression is well typed" $ do
      shouldFailAssertion (shouldNotCompile ("foo" :: String))
