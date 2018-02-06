-- https://www.codewars.com/kata/array-dot-diff

module Difference where

difference :: Eq a => [a] -> [a] -> [a]
difference a b = [i | i <- a, i `notElem` b]
