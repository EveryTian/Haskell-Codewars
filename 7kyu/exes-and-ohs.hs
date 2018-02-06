-- https://www.codewars.com/kata/exes-and-ohs

module Codewars.Kata.XO where
import Data.Char
-- | Returns true if the number of
-- Xs is equal to the number of Os
-- (case-insensitive)
xo :: String -> Bool
xo str = let s = map toLower str
         in s `count` 'o' == s `count` 'x'
         where count s c = length [i | i <- s, i == c]
