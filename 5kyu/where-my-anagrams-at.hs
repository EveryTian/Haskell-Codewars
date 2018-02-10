-- https://www.codewars.com/kata/where-my-anagrams-at

module Anagram where

import Data.List (sort)

anagrams :: String -> [String] -> [String]
anagrams w = let sortW = sort w in filter (\ x -> sortW == sort x)
