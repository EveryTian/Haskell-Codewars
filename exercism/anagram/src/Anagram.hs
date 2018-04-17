module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss =
    let s1 = map toLower xs
        s2 = sort s1
    in [i | i <- xss, let x1 = map toLower i
                          x2 = sort x1
                      in s2 == x2 && s1 /= x1]
