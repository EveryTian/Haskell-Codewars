-- https://www.codewars.com/kata/century-from-year

module Century where

century::Int -> Int
century = ceiling . (/ 100) . fromIntegral
