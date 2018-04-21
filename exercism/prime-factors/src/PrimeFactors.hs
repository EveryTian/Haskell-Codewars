module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors = primeFactors' primes []
    where primeFactors' :: [Integer] -> [Integer] -> Integer -> [Integer]
          primeFactors' ps@(x:xs) result n
              | n == 1 = reverse result
              | n `mod` x == 0 = primeFactors' ps (x:result) (n `div` x)
              | otherwise = primeFactors' xs result n

primes = 2 : 3 : filter (\ n -> all ((0 /=) . mod n) $ takeWhile (\ i -> i * i <= n) primes) [5, 7 ..]
