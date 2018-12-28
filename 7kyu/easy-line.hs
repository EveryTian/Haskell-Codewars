-- https://www.codewars.com/kata/easy-line

module Codewars.Kata.Easyline where

easyLine :: Integer -> Integer
easyLine n = product [n + 1 .. 2 * n] `div` product [2 .. n]
