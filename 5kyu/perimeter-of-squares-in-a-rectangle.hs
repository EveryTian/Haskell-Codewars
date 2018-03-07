-- https://www.codewars.com/kata/perimeter-of-squares-in-a-rectangle

module Codewars.Kata.Perimeter where

perimeter :: Integer -> Integer
perimeter = (* 4) . sum . reveredFib . (\x -> x + 1)
    where addOneToReversedFib :: [Integer] -> [Integer]
          addOneToReversedFib [] = [1]
          addOneToReversedFib [_] = [1, 1]
          addOneToReversedFib xss@(x0:x1:xs) = (x0 + x1):xss
          reveredFib :: Integer -> [Integer]
          reveredFib n = reveredFib' n []
          reveredFib' :: Integer -> [Integer] -> [Integer]
          reveredFib' 0 arr = arr
          reveredFib' n arr = reveredFib' (n - 1) (addOneToReversedFib arr)
