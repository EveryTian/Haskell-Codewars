-- https://www.codewars.com/kata/convert-a-linked-list-to-a-string

module Kata where

stringify :: [Int] -> String
stringify [] = "null"
stringify (x:xs) = show x ++ " -> " ++ stringify xs
