-- https://www.codewars.com/kata/fibo-akin

module Codewars.G964.Fibkind where

u :: [Int]
u = 0 : 1 : 1 : zipWith (+) (f 1) (f 2)
    where f x = map (u !!) $ zipWith (-) [3..] (drop x u)

lengthSupUK :: Int -> Int -> Int
lengthSupUK n k = sum [1 | i <- take (n + 1) u, i >= k]

comp :: Int -> Int
comp n = let un = take (n + 1) u
         in length $ filter id $ zipWith (>) un (tail un)
