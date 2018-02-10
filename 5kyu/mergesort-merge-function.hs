-- https://www.codewars.com/kata/mergesort-merge-function

module Merge where

merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = f xs ys []
    where f xs [] r = r ++ xs
          f [] ys r = r ++ ys
          f (x:xs) (y:ys) r
              | x <= y    = f xs (y:ys) (r ++ [x])
              | otherwise = f (x:xs) ys (r ++ [y])
