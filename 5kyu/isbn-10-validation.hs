-- https://www.codewars.com/kata/isbn-10-validation

module ISBN10 where

import Data.Char(ord, chr)

validISBN10 :: String -> Bool
validISBN10 isbn
    | length isbn /= 10 = False
    | otherwise = let isbnIntList = map f isbn
                      check = last isbnIntList
                      list = init isbnIntList
                  in (length . filter (\ x -> 0 <= x && x <= 9) $ list) == 9 && 
--                     (sum . map (\ x -> fst x * snd x) . zip [1..10] $ list) `mod` 11 == check
--                     (sum . map (uncurry (*)) . zip [1..10] $ list) `mod` 11 == check
                     (sum . zipWith (*) [1..10] $ list) `mod` 11 == check

f :: Char -> Int
f 'X' = 10
f c = ord c - ord '0'
