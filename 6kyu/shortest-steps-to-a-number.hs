-- https://www.codewars.com/kata/shortest-steps-to-a-number

module ShortestSteps (steps) where

steps :: Int -> Int
steps 1 = 0
steps n
  | odd n = 1 + steps (n - 1)
  | otherwise = 1 + steps (n `div` 2)
