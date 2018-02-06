-- https://www.codewars.com/kata/number-of-trailing-zeros-of-n

module Zeros where

zeros :: Int -> Int
zeros x = fun x (x `div` 5) 0 where fun aN n num = if n == 0 then num else fun n (n `div` 5) (num + n)
