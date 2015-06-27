{-# LANGUAGE ScopedTypeVariables #-}

module Test.ShouldNotTypecheck (shouldNotTypecheck) where

import Control.Exception (evaluate, try, throw, ErrorCall(..))
import Data.List (isSuffixOf)
import Test.HUnit.Lang (Assertion, assertFailure)

shouldNotTypecheck :: forall a. a -> Assertion
shouldNotTypecheck a = do
  result <- try (evaluate a)
  case result of
    Right _ -> assertFailure "Expected expression to not compile but it did compile"
    Left (ErrorCall msg) -> case isSuffixOf "(deferred type error)" msg of
      True -> return ()
      False -> throw (ErrorCall msg)
