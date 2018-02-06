-- https://www.codewars.com/kata/vowel-count

module Codewars.Kata.Vowel where

getCount :: String -> Int
getCount str = length [i | i <- str, i `elem` "aeiou"]
