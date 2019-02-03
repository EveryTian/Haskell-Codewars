-- https://www.codewars.com/kata/highest-and-lowest

module Kata (highAndLow) where

highAndLow :: String -> String
highAndLow input = show (maximum ns) ++ (' ' : show (minimum ns))
    where ns :: [Int]
          ns = map read $ words input
