-- https://www.codewars.com/kata/largest-5-digit-number-in-a-series

module LargestDigits where

import Data.Char
digit5 :: String -> Int
digit5 xs 
    | length xs <= 5 = strToNum 0 xs
    | otherwise = strToNum 0 . maximum $ [take 5 $ drop i xs | i <- [0 .. (length xs - 5)]]
          where strToNum n "" = n
                strToNum n (x:xs) = strToNum (n * 10 + (ord x) - (ord '0')) xs
