-- https://www.codewars.com/kata/roman-numerals-decoder

module Roman where

import Data.List (group)

getValue :: Char -> Int
getValue 'I' = 1
getValue 'V' = 5
getValue 'X' = 10
getValue 'L' = 50
getValue 'C' = 100
getValue 'D' = 500
getValue 'M' = 1000

solution :: String -> Int
solution = calcSum . map (\ x -> length x * getValue (head x)) . group
    where calcSum :: [Int] -> Int
          calcSum [] = 0
          calcSum [x] = x
          calcSum (x0:x1:xs)
              | x0 < x1 = negate x0 + calcSum (x1:xs)
              | otherwise = x0 + calcSum (x1:xs)
