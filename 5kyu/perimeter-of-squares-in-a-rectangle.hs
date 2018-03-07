-- https://www.codewars.com/kata/perimeter-of-squares-in-a-rectangle

module Codewars.Kata.Perimeter where

perimeter :: Integer -> Integer
perimeter = (* 4) . sum . reversedFib . (\x -> x + 1)
    where addOneToReversedFib :: [Integer] -> [Integer]
          addOneToReversedFib [] = [1]
          addOneToReversedFib [_] = [1, 1]
          addOneToReversedFib xss@(x0:x1:xs) = (x0 + x1):xss
          reversedFib :: Integer -> [Integer]
          reversedFib n = reversedFib' n []
          reversedFib' :: Integer -> [Integer] -> [Integer]
          reversedFib' 0 arr = arr
          reversedFib' n arr = reversedFib' (n - 1) (addOneToReversedFib arr)
