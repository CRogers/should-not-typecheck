{-# LANGUAGE GADTs, TemplateHaskell #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

import Control.DeepSeq
import Control.Exception
import GHC.Generics (Generic)
import Test.Hspec
import Test.Hspec.Expectations (expectationFailure)
import Test.HUnit.Lang (performTestCase)
import Test.ShouldNotTypecheck

shouldFailAssertion :: IO () -> IO ()
shouldFailAssertion value = do
  result <- performTestCase value
  case result of
    Nothing           -> expectationFailure "Did not throw an assertion error"
    Just (True,  _)   -> return ()
    Just (False, msg) -> expectationFailure $ "Raised an error " ++ msg

shouldThrowException :: Exception e => e -> IO () -> IO ()
shouldThrowException exception value = do
  result <- performTestCase value
  case result of
    Nothing           -> expectationFailure "Did not throw exception: assertion succeeded"
    Just (True,  _)   -> expectationFailure "Did not throw exception: assertion failed"
    Just (False, msg) -> case msg == show exception of
      True -> return ()
      False -> expectationFailure "Incorrect exception propagated"

data Expr t where
  IntVal :: Int -> Expr Int
  BoolVal :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int

instance NFData (Expr t) where
  rnf expr = case expr of
    IntVal i -> rnf i
    BoolVal b -> rnf b
    Add l r -> rnf l `seq` rnf r

main :: IO ()
main = hspec $ do
  describe "shouldNotCompile" $ do
    it "should not throw an assertion error when an expression is ill typed" $ do
      shouldNotTypecheck ("foo" :: Int)

    it "should throw an assertion error when an expression is well typed" $ do
      shouldFailAssertion (shouldNotTypecheck ("foo" :: String))

    it "should throw an actual exception and not fail the assertion if the expression contains an non-HUnitFailure exception" $ do
      let exception = NoMethodError "lol"
      shouldThrowException exception (shouldNotTypecheck (throw exception :: Int))

    it "should propagate an actual exception and not fail the assertion if the expression contains a non-deferred ErrorCall exception" $ do
      let exception = ErrorCall "yay"
      shouldThrowException exception (shouldNotTypecheck (throw exception :: Int))

    it "should not throw an assertion when an expression with more than one level of constructors is ill typed" $ do
      shouldNotTypecheck (Add (BoolVal True) (IntVal 4))
