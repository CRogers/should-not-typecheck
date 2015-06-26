{-# OPTIONS_GHC -fdefer-type-errors #-}

module Main where

import Test.Hspec
import TypeTest

main :: IO ()
main = hspec $ do
  describe "Type Test" $ do
    it "should throw an assertion error when an expression is ill typed" $ do
      shouldNotCompile ("foo" :: Int)
