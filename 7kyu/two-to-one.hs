-- https://www.codewars.com/kata/two-to-one

module Codewars.G964.Longest where

import Data.List (group, sort)

longest :: [Char] -> [Char] -> [Char]
longest s1 s2 = let s = sort $ s1 ++ s2
                in map (!! 0) $ group s
