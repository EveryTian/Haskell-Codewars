-- https://www.codewars.com/kata/ready-for-prime-time

module PrimeTime where

prime :: Int -> [Int]
prime n
  | n <= 1 = []
  | n == 2 = [2]
  | otherwise = reverse $ addItem 3 [2] n
      where addItem x retArr n
              | x > n = retArr
              | otherwise = if isPrime x $ reverse retArr
                            then addItem (x + 2) (x:retArr) n 
                            else addItem (x + 2) retArr n
            isPrime _ [] = True
            isPrime n (x:xs) 
              | x * x > n = True
              | n `mod` x == 0 = False
              | otherwise = isPrime n xs
