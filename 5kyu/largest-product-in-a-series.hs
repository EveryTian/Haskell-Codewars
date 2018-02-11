-- https://www.codewars.com/kata/largest-product-in-a-series

module LargestProduct where

import Data.Char (digitToInt)

greatestProduct :: String -> Int
greatestProduct str = f (map digitToInt str) 0
    where f (x1:x2:x3:x4:[]) max = max
          f (x1:x2:x3:x4:x5:xs) max
              | x1*x2*x3*x4*x5 > max = f (x2:x3:x4:x5:xs) (x1*x2*x3*x4*x5)
              | otherwise = f (x2:x3:x4:x5:xs) max
