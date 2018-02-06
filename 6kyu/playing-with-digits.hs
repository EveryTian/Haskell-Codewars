-- https://www.codewars.com/kata/playing-with-digits

module Codewars.Kata.DigPow where
import Data.Char
digpow :: Integer -> Integer -> Integer
digpow n p = let intList = map (\ x -> ord x - ord '0') $ show n
                 sumAll s _ [] = s
                 sumAll s curp (x:xs) = sumAll (s + x ^ curp) (curp + 1) xs
                 sumOfAll = sumAll 0 p intList
             in if sumOfAll `mod` fromInteger n == 0 then toInteger $ sumOfAll `div` fromInteger n else -1