-- https://www.codewars.com/kata/folding-your-way-to-the-moon

module Codewars.Kata.Fold where

foldTo :: Double -> Maybe Int
foldTo distance
    | distance <= 0 = Nothing
    | distance <= 0.0001 = Just 0
    | otherwise = 1 +++ foldTo (distance / 2)
          where x +++ Just y = Just (x + y)
