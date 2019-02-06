-- https://www.codewars.com/kata/n-th-fibonacci

module Fib where

fib :: Int -> Int
fib n = fibList n !! (n - 1)
fibList :: Int -> [Int]
fibList 1 = [0]
fibList 2 = [0, 1]
fibList n = listN ++ [(listN !! (n - 3)) + (listN !! (n - 2))]
    where listN = fibList (n - 1)
