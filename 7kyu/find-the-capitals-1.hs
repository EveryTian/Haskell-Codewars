-- https://www.codewars.com/kata/find-the-capitals-1

module Codewars.Kata.Capitals where

import Data.Char (isUpper)

capitals :: String -> [Int]
capitals = f 0
    where f :: Int -> String -> [Int]
          f _ "" = []
          f index (x:xs)
              | isUpper x = index:(f (index + 1) xs)
              | otherwise = f (index + 1) xs
