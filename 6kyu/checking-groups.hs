-- https://www.codewars.com/kata/checking-groups

module Codewars.Kata.Groups where

groupCheck :: String -> Bool
groupCheck = stack ""
    where stack s "" = length s == 0
          stack s (x:xs)
              | x `elem` "({[" = stack (x:s) xs
              | x == ')' = if length s > 0 && '(' == (s !! 0) then stack (drop 1 s) xs else False
              | x == ']' = if length s > 0 && '[' == (s !! 0) then stack (drop 1 s) xs else False
              | x == '}' = if length s > 0 && '{' == (s !! 0) then stack (drop 1 s) xs else False
