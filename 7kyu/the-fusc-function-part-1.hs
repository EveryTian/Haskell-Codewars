-- https://www.codewars.com/kata/the-fusc-function-part-1

module Fusc where

fusc :: Int -> Int
fusc 0 = 0
fusc 1 = 1
fusc m
  | even m = let n = div m 2 in fusc n
  | otherwise = let n = (m - 1) `div` 2 in fusc n + fusc (n + 1)
