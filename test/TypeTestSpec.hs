{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

import Control.Exception
import Test.Hspec
import Test.Hspec.Expectations (expectationFailure)
import Test.HUnit.Lang (performTestCase)
import TypeTest

main :: IO ()
main = hspec $ do
  describe "Type Test" $ do
    it "should not throw an assertion error when an expression is ill typed" $ do
      shouldNotCompile ("foo" :: Int)

    it "should throw an assertion error when an expression is well typed" $ do
      result <- performTestCase (shouldNotCompile ("foo" :: String))
      case result of
        Just (True, _) -> return ()
        Just (False, msg) -> expectationFailure $ "Raised an error " ++ msg
        Nothing -> expectationFailure "Did not throw an assertion error"
