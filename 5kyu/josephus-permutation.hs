-- https://www.codewars.com/kata/josephus-permutation

module Josephus where

josephus :: [a] -> Int -> [a]
josephus [] _ = []
josephus [x] _ = [x]
josephus xs k = let (xs', n) = joseOnce xs k
                in n : josephus xs' k

joseOnce :: [a] -> Int -> ([a], a)
joseOnce xs k = case k `rem` length xs
                of 0 -> (init xs, last xs)
                   k' -> (drop k' xs ++ take (k' - 1) xs, xs !! (k' - 1))
