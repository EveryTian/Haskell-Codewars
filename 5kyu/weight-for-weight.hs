-- https://www.codewars.com/kata/weight-for-weight

module Codewars.G964.WeightSort where

import Data.Char

orderWeight :: String -> String
orderWeight str = foldl1 (\ x y -> x ++ ' ':y) $ qsort $ words str
    where qsort [] = []
          qsort (x:xs) = qsort [i | i <- xs, i `lessWeight` x] ++ [x] ++ qsort [i | i <- xs, not (i `lessWeight` x)]
          weight = sum . map (\ x -> ord x - ord '0')
          lessWeight a b
              | weight a == weight b = a < b
              | otherwise = weight a < weight b
