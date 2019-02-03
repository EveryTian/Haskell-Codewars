-- https://www.codewars.com/kata/sum-of-positive

module Codewars.Arrays where

positiveSum :: [Int] -> Int
positiveSum = sum . filter (> 0)
