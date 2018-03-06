-- https://www.codewars.com/kata/anagram-detection

module Codewars.Anagram where

import Data.Char (toLower)
import Data.List (sort)

isAnagramOf :: String -> String -> Bool
isAnagramOf test original = let f = sort . map toLower
                            in f test == f original
