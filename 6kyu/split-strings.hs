-- https://www.codewars.com/kata/split-strings

module Codewars.Kata.SplitStrings where

solution :: String -> [String]
solution = solution' [] ""
    where solution' ret "" "" = ret
          solution' ret temp "" = case length temp of
              2 -> ret ++ [temp]
              1 -> ret ++ [temp ++ ['_']]
          solution' ret temp (x:xs) = case length temp of
              2 -> solution' (ret ++ [temp]) [x] xs
              1 -> solution' (ret ++ [temp ++ [x]]) "" xs
              0 -> solution' ret [x] xs
