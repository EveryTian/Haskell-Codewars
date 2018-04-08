-- https://www.codewars.com/kata/happy-numbers-2

module IsHappy where

import Data.Char (digitToInt)

isHappy :: Integer -> Bool
isHappy 1 = True
isHappy 4 = False
isHappy n = isHappy . fromIntegral . sum . map ((^ 2) . digitToInt) . show $ n
