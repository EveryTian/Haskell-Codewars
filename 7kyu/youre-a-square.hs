-- https://www.codewars.com/kata/youre-a-square

module Codewars.Kata.Square where

isSquare :: Integral n => n -> Bool
isSquare n = let sqrtN = floor $ sqrt $ fromIntegral n
             in sqrtN * sqrtN == n
