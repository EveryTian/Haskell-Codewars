-- https://www.codewars.com/kata/area-of-a-circle

module Circle where

circleArea :: Double -> Maybe Double
circleArea x
    | x <= 0 = Nothing
    | otherwise = Just (x * x * pi)
