-- https://www.codewars.com/kata/equal-sides-of-an-array

module Codewars.G964.FindEven where

findEvenIndex :: [Int] -> Int
findEvenIndex [] = -1
findEvenIndex arr = test 0 arr (length arr) (sum arr)
  where test i a l s
            | i >= l = -1
            | let t = sum $ take i a in t == s - (a !! i) - t = i
            | otherwise = test (i + 1) a l s
