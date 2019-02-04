-- https://www.codewars.com/kata/substituting-variables-into-strings-padded-numbers

module Codewars.Kata.PaddedNumbers where

solution :: Int -> String
solution n = let str = show n
             in "Value is " ++ replicate (5 - length str) '0' ++ str
