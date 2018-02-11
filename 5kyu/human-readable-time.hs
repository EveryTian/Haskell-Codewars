-- https://www.codewars.com/kata/human-readable-time

module HumanTime where

humanReadable :: Int -> String
humanReadable x = let imp str = if length str == 1 then '0':str else str
    in (imp . show $ (x `div` 3600)) ++ [':']
      ++ (imp . show $ (x `div` 60 `mod` 60)) ++ [':']
      ++ (imp . show $ (x `mod` 60))
