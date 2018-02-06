-- https://www.codewars.com/kata/remove-the-minimum

module Codewars.Kata.RemoveSmallest where

removeSmallest :: [Int] -> [Int]
removeSmallest arr = let smallest = minimum arr
                         idx [] _ _ = -1
                         idx (x:xs) itm i = if x == itm then i else idx xs itm i + 1
                         minIdx = idx arr smallest 0
                     in (take minIdx arr) ++ (drop (minIdx + 1) arr)
