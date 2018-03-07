-- https://www.codewars.com/kata/product-of-consecutive-fib-numbers

module Codewars.Kata.Fib where

-- | Returns a pair of consecutive Fibonacci numbers a b,
--   where (a*b) is equal to the input, or proofs that the
--   number isn't a product of two consecutive Fibonacci 
--   numbers.
productFib :: Integer -> (Integer, Integer, Bool)
productFib = test 0 1
    where test :: Integer -> Integer -> Integer -> (Integer, Integer, Bool)
          test x y n
              | x * y == n = (x, y, True)
              | x * y > n = (x, y, False)
              | otherwise = test y (x + y) n
