-- https://www.codewars.com/kata/split-strings

module Codewars.Kata.SplitStrings where

solution :: String -> [String]
solution = solution' [] ""
    where solution' ret "" "" = ret
          solution' ret temp ""
              | length temp == 2 = ret ++ [temp]
              | length temp == 1 = ret ++ [temp ++ ['_']]
          solution' ret temp (x:xs)
              | length temp == 2 = solution' (ret ++ [temp]) [x] xs
              | length temp == 1 = solution' (ret ++ [temp ++ [x]]) "" xs
              | length temp == 0 = solution' ret [x] xs
