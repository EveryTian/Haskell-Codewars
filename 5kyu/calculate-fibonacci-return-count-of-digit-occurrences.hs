-- https://www.codewars.com/kata/calculate-fibonacci-return-count-of-digit-occurrences

module Kata (fibDigits) where

import Data.List (sortBy)

fibDigits :: Integer -> [(Integer, Integer)]
fibDigits = sortBy (flip compare) . filter ((/= 0) . fst) . (`zip` [0..9]) . count . show . (fibs !!) . fromIntegral
  where
    count = count' $ replicate 10 0
      where
        count' result [] = result
        count' [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] ('0':xs) = count' [n0 + 1, n1, n2, n3, n4, n5, n6, n7, n8, n9] xs
        count' [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] ('1':xs) = count' [n0, n1 + 1, n2, n3, n4, n5, n6, n7, n8, n9] xs
        count' [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] ('2':xs) = count' [n0, n1, n2 + 1, n3, n4, n5, n6, n7, n8, n9] xs
        count' [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] ('3':xs) = count' [n0, n1, n2, n3 + 1, n4, n5, n6, n7, n8, n9] xs
        count' [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] ('4':xs) = count' [n0, n1, n2, n3, n4 + 1, n5, n6, n7, n8, n9] xs
        count' [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] ('5':xs) = count' [n0, n1, n2, n3, n4, n5 + 1, n6, n7, n8, n9] xs
        count' [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] ('6':xs) = count' [n0, n1, n2, n3, n4, n5, n6 + 1, n7, n8, n9] xs
        count' [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] ('7':xs) = count' [n0, n1, n2, n3, n4, n5, n6, n7 + 1, n8, n9] xs
        count' [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] ('8':xs) = count' [n0, n1, n2, n3, n4, n5, n6, n7, n8 + 1, n9] xs
        count' [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9] ('9':xs) = count' [n0, n1, n2, n3, n4, n5, n6, n7, n8, n9 + 1] xs
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
