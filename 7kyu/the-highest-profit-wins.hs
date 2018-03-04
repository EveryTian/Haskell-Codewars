-- https://www.codewars.com/kata/the-highest-profit-wins

module Codewars.Kata.MinMax where

-- | Takes a non-empty list and returns
--   both maximum and minimum value
minMax :: (Ord a) => [a] -> (a, a)
minMax xs = (minimum xs, maximum xs)
