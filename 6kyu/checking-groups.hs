-- https://www.codewars.com/kata/checking-groups

module Codewars.Kata.Groups where

groupCheck :: String -> Bool
groupCheck = stack ""
    where stack s "" = null s
          stack s (x:xs)
              | x `elem` "({[" = stack (x:s) xs
              | x == ')' = not (null s) && '(' == head s && stack (tail s) xs
              | x == ']' = not (null s) && '[' == head s && stack (tail s) xs
              | x == '}' = not (null s) && '{' == head s && stack (tail s) xs
