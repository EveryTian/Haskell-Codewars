-- https://www.codewars.com/kata/valid-braces

module Codewars.Kata.Braces where

validBraces :: String -> Bool
validBraces xs = f xs ""
f "" stack = length stack == 0
f (']':xs) stack = if length stack > 0 && stack !! 0 == '[' then f xs (drop 1 stack) else False
f ('}':xs) stack = if length stack > 0 && stack !! 0 == '{' then f xs (drop 1 stack) else False
f (')':xs) stack = if length stack > 0 && stack !! 0 == '(' then f xs (drop 1 stack) else False
f (x:xs) stack = f xs (x:stack)
