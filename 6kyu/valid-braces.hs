-- https://www.codewars.com/kata/valid-braces

module Codewars.Kata.Braces where

validBraces :: String -> Bool
validBraces xs = f xs ""
f "" stack = null stack
f (']':xs) stack = not (null stack) && head stack == '[' && f xs (tail stack)
f ('}':xs) stack = not (null stack) && head stack == '{' && f xs (tail stack)
f (')':xs) stack = not (null stack) && head stack == '(' && f xs (tail stack)
f (x:xs) stack = f xs (x:stack)
