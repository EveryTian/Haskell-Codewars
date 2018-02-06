-- https://www.codewars.com/kata/is-a-number-prime

module IsPrime where

isPrime :: Integer -> Bool
isPrime 2 = True
isPrime x
    | x <= 1 = False
    | x `mod` 2 == 0 = False
    | otherwise = checkFrom 3 x
          where checkFrom n x
                    | x `mod` n == 0 = False
                    | n * n > x = True
                    | otherwise = checkFrom (n + 2) x
