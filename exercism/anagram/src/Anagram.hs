module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [i | i <- xss, 
    x <- pure $ map toLower i,
    s <- pure $ map toLower xs, 
    sort s == sort x && s /= x]
