-- https://www.codewars.com/kata/fizz-slash-buzz

module FizzBuzz where

fizzbuzz :: Int -> [Int]
fizzbuzz n = [f3', f5', f15]
    where f x = length [i | i <- [1..n - 1], i `mod` x == 0]
          f3' = let f3 = f 3 in if f3 == 0 then 0 else f3 - f15
          f5' = let f5 = f 5 in if f5 == 0 then 0 else f5 - f15
          f15 = f 15
