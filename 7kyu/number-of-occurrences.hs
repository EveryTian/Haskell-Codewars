-- https://www.codewars.com/kata/number-of-occurrences

module Codewars.Occurrences where

numberOfOccurrences :: Eq a => a -> [a] -> Int
numberOfOccurrences x = length . filter (== x)
