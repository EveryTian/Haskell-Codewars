-- https://www.codewars.com/kata/isograms

module Isogram where
import Data.List
import Data.Char
isIsogram :: String -> Bool
isIsogram s = length [1 | y <- (group . sort . map toUpper) s, length y /= 1] == 0
