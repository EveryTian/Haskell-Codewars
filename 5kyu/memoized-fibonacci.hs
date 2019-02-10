-- https://www.codewars.com/kata/memoized-fibonacci

module Fibonacci where

fibonacci :: Int -> Integer
-- fibonacci n = fibList n !! n
--     where fibList 0 = [0]
--           fibList 1 = [0, 1]
--           fibList n = listN ++ [(listN !! (n - 1)) + (listN !! (n - 2))]
--               where listN = fibList (n - 1)
fibonacci n = fibs !! n

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
