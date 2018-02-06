-- https://www.codewars.com/kata/find-the-vowels

module VowelIndices where

vowelIndices :: String -> [Integer]
vowelIndices word = [toInteger $ i + 1 | i <- [0 .. length word - 1], word !! i `elem` "aeiouAEIOUyY"]
