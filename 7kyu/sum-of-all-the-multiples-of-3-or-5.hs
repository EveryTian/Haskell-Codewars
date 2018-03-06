-- https://www.codewars.com/kata/sum-of-all-the-multiples-of-3-or-5

module SumOfMultiples where

findSum n = sum [i | i <- [1..n], i `mod` 3 == 0 || i `mod` 5 == 0]
