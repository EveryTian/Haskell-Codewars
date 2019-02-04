-- https://www.codewars.com/kata/thinking-and-testing-uniq-or-not-uniq

module Codewars.Puzzle (testit) where

import Data.List (sort, group)

testit :: [Int] -> [Int] -> [Int]
testit a b = let f = map head . group . sort in sort (f a ++ f b)
