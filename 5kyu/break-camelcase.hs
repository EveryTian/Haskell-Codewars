-- https://www.codewars.com/kata/break-camelcase

module Codewars.Kata.BreakCamelCase where

import Data.Char(isUpper)

solution :: String -> String
solution = g . f

f :: String -> String
f "" = ""
f (x:xs)
    | isUpper x = ' ':x:(f xs)
    | otherwise = x:(f xs)

g :: String -> String
g (' ':xs) = g xs
g str = str
