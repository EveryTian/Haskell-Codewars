-- https://www.codewars.com/kata/dont-give-me-five

module Kata where

dontGiveMeFive :: Int -> Int -> Int
dontGiveMeFive start end = let boolList = map (\ x -> '5' `elem` show x) [start..end] in foldl (\ num x -> if not x then num + 1 else num) 0 boolList
