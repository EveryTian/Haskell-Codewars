-- https://www.codewars.com/kata/multiplying-numbers-as-strings

module MultNumAsStrings where

multiply :: String -> String -> String
multiply xs ys = show $ read xs * read ys
