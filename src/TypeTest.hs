{-# OPTIONS_GHC -fdefer-type-errors #-}

module TypeTest where

import Control.Exception

a = "cat" :: Int

test :: IO String
test = do
  result <- try (evaluate a)
  return $ case result of
    Right v -> "It succeeded with value " ++ show v
    Left e -> "It failed:\n" ++ show (e :: SomeException)
