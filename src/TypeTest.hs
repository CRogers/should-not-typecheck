{-# LANGUAGE ScopedTypeVariables #-}

module TypeTest where

import Control.Exception (evaluate, try, throw, ErrorCall(..))
import Data.List (isSuffixOf)
import Test.Hspec.Expectations (Expectation, expectationFailure)

shouldNotCompile :: forall a. a -> Expectation
shouldNotCompile a = do
  result <- try (evaluate a)
  case result of
    Right _ -> expectationFailure "Expected expression to not compile but it did compile"
    Left (ErrorCall msg) -> case isSuffixOf "(deferred type error)" msg of
      True -> return ()
      False -> throw (ErrorCall msg)
