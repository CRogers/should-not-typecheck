{-# OPTIONS_GHC -fdefer-type-errors #-}

module TypeTest where

import Control.Exception
import Test.Hspec

thing :: Show a => a -> IO String
thing a = do
  result <- try (evaluate a)
  return $ case result of
    Right v -> "It succeeded with value " ++ show v
    Left e -> "It failed:\n" ++ show (e :: SomeException)

test :: IO ()
test = hspec $ do
  describe "lol" $ do
    it "should not let me make a string an int" $ do
      thing ("cat" :: Int) >>= (`shouldBe` "cat")
