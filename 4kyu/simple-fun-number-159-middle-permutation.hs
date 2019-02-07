-- https://www.codewars.com/kata/simple-fun-number-159-middle-permutation

module MiddlePermutation.JorgeVS.Kata where

import Data.List (permutations, sort)

middlePermutation :: String -> String
middlePermutation myString  = sort (permutations myString) !! (fact (length myString) `div` 2 - 1)
    where fact 0 = 1
          fact n = n * fact (n - 1)
