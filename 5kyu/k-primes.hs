-- https://www.codewars.com/kata/k-primes

module Codewars.G964.Kprimes where

countKprimes :: Int -> Int -> Int -> [Int]
countKprimes k start end = filter ((== k) . length . primeFactors) [start..end]

primeFactors :: Int -> [Int]
primeFactors = primeFactors' primes []
    where primeFactors' ps@(x:xs) result n
              | n == 1 = reverse result
              | n `mod` x == 0 = primeFactors' ps (x:result) (n `div` x)
              | otherwise = primeFactors' xs result n

primes :: [Int]
primes = 2 : 3 : filter (\ n -> all ((0 /=) . mod n) $ takeWhile (\ i -> i * i <= n) primes) [5, 7 ..]

puzzle :: Int -> Int
puzzle s = let onePrimes = takeWhile (< s) primes
               threePrimes = countKprimes 3 8 (s - 1)
               sevenPrimes = countKprimes 7 128 (s - 1)
           in sum [1 | p1 <- onePrimes, p3 <- threePrimes, p7 <- sevenPrimes, p1 + p3 + p7 == s]
