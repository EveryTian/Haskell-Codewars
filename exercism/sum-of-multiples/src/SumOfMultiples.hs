module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ filter (f factors) [1 .. limit - 1]
    where f [] n = False
          f (x:xs) n
              | n `mod` x == 0 = True
              | otherwise = f xs n
