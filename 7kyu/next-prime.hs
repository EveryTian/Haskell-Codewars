-- https://www.codewars.com/kata/next-prime

module Codewars.NextPrime where

nextPrime :: Integer -> Integer
nextPrime 1 = 2
nextPrime n
    | odd n = getFirstPrime [n + 2, n + 4 ..]
    | even n = getFirstPrime [n + 1, n + 3 ..]
          where getFirstPrime (x:xs)
                    | isPrime x = x
                    | otherwise = getFirstPrime xs
                isPrime x = isPrime' x 3 $ floor $ sqrt $ fromIntegral x
                isPrime' x n max
                    | n > max = True
                    | x `mod` n == 0 = False
                    | otherwise = isPrime' x (n + 2) max
