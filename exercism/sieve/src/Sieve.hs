module Sieve (primesUpTo) where

primesUpTo :: Integer -> [Integer]
primesUpTo n = takeWhile (<= n) primes

primes = 2 : 3 : filter (\ n -> all ((0 /=) . mod n) $ takeWhile (\ i -> i * i <= n) primes) [5, 7 ..]
