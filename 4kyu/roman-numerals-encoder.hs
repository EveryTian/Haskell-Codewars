-- https://www.codewars.com/kata/roman-numerals-encoder

module RomanNumerals where

import Data.Maybe (fromMaybe)

solution :: Integer -> String
solution = fromMaybe "" . numerals . fromIntegral
    
numerals :: Int -> Maybe String
numerals n
    | n <= 0 || n > 3999 = Nothing
    | n < 10 = Just $ numeral n 'I' 'V' 'X'
    | n < 100 = Just $ numeral (n `div` 10) 'X' 'L' 'C' ++ (fromMaybe "" . numerals $ n `mod` 10)
    | n < 1000 = Just $ numeral (n `div` 100) 'C' 'D' 'M' ++ (fromMaybe "" . numerals $ n `mod` 100)
    | otherwise = Just $ numeral (n `div` 1000) 'M' ' ' ' ' ++ (fromMaybe "" . numerals $ n `mod` 1000)
    where numeral :: Int -> Char -> Char -> Char -> String
          numeral n c1 c5 c10
              | n == 0 = ""
              | n <= 3 = replicate n c1
              | n == 4 = [c1, c5]
              | n <= 8 = c5 : replicate (n - 5) c1
              | n == 9 = [c1, c10]
