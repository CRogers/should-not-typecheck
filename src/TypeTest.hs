{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module TypeTest where

import Control.Exception
import Test.Hspec
import Test.Hspec.Expectations (expectationFailure)

shouldNotCompile :: forall a. a -> IO ()
shouldNotCompile a = do
  result <- try (evaluate a) :: IO (Either ErrorCall a)
  case result of
    Right _ -> expectationFailure "Expected expression to not compile but it did compile"
    Left _ -> return ()

test :: IO ()
test = hspec $ do
  describe "lol" $ do
    it "should not let me make a string an int" $ do
      shouldNotCompile ("cat" :: String)
