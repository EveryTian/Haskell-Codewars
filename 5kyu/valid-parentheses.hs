-- https://www.codewars.com/kata/valid-parentheses

module Codewars.Parentheses where

validParentheses :: String -> Bool
validParentheses = stack ""
    where stack s "" = length s == 0
          stack s (x:xs)
              | x == '(' = stack (x:s) xs
              | x == ')' = if length s > 0 && '(' == (s !! 0) then stack (drop 1 s) xs else False
