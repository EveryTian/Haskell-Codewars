-- https://www.codewars.com/kata/custom-sort-function

module Codewars.Kata.Sort where

sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = sort [i | i <- xs, i <= x] ++ [x] ++ sort [i | i <- xs, i > x]
