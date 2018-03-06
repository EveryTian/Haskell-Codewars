-- https://www.codewars.com/kata/sequencesum

module SequenceSum where

sumOfN :: Int -> [Int]
sumOfN 0 = [0]
sumOfN n
    | n < 0 = let x = sumOfN (n + 1)
              in x ++ [last x + n]
    | n > 0 = let x = sumOfN (n - 1)
              in x ++ [last x + n]
