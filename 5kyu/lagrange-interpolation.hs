-- https://www.codewars.com/kata/lagrange-interpolation

module Codewars.Jacobb.Lagrange where

lagrange :: (Eq a, Fractional a) => [(a, a)] -> a -> a
lagrange points x = sum [y1 * product [(x - x2) / (x1 - x2) | point2@(x2, y2) <- points, point1 /= point2] | point1@(x1, y1) <- points]
