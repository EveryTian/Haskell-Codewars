module Grains (square, total) where

import Data.Maybe (fromMaybe)

square :: Integer -> Maybe Integer
square n
  | n <= 0 || n > 64 = Nothing
  | otherwise = Just $ 2 ^ (n - 1)

total :: Integer
total = sum $ map (fromMaybe 0 . square) [1..64]
