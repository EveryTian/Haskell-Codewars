-- https://www.codewars.com/kata/adjacent-pairs-in-a-string

module AdjacentPairs where

import Data.List(group)

countAdjacentPairs :: String -> Int
countAdjacentPairs str = length $ filter (\ x -> length x > 1) ((group . words) str)
