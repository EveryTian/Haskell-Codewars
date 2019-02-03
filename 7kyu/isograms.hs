-- https://www.codewars.com/kata/isograms

module Isogram where

import Data.List (sort, group)
import Data.Char (toUpper)

isIsogram :: String -> Bool
isIsogram s = null [1 | y <- (group . sort . map toUpper) s, length y /= 1]
