-- https://www.codewars.com/kata/all-balanced-parentheses

module Balanced.Parens where

import Data.Bits (popCount, testBit)

balancedParens :: Int -> [String]
balancedParens n = let bottom = 2 ^ n - 1
                       top = (4 ^ n - 1) `div` 3
                   in filter (check 0) $ map bracketify $ filter ((== n) . popCount) [bottom, bottom + 2 .. top]
    where bracketify :: Int -> String
          bracketify x = map (\y -> if testBit x y then ')' else '(') [2 * n - 1, 2 * n - 2 .. 0]
          check 0 [] = True
          check _ [] = False
          check n ('(':xs) = check (n + 1) xs
          check n (')':xs) = n - 1 >= 0 && check (n - 1) xs
