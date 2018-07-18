-- https://www.codewars.com/kata/are-the-numbers-in-order

module NumbersInOrder where

import Data.List (sort)

isAscOrder :: [Int] -> Bool
isAscOrder = sort >>= (==)
