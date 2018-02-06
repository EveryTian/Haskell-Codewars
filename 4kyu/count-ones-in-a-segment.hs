-- https://www.codewars.com/kata/count-ones-in-a-segment

module CountOnes where

countOnes :: Integer -> Integer -> Integer
countOnes left right = g right - g (left - 1)
    where g 0 = 0
          g n
            | odd n = 2 * g (n `div` 2) + n `div` 2 + 1
            | otherwise = 2 * g (n `div` 2 - 1) + n `div` 2 + f (n `div` 2)
          f 0 = 0
          f n
            | even n = f (n `div` 2)
            | otherwise = f (n `div` 2) + 1
