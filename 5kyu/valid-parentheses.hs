-- https://www.codewars.com/kata/valid-parentheses

module Codewars.Parentheses where

validParentheses :: String -> Bool
validParentheses = stack ""
    where stack s "" = null s
          stack s (x:xs)
              | x == '(' = stack (x:s) xs
              | x == ')' = not (null s) && '(' == head s && stack (tail s) xs
