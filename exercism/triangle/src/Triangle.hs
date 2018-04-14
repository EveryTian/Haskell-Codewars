module Triangle (TriangleType(..), triangleType) where

import Data.List (sort)

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
    | a <= 0 || b <= 0 || c <= 0 = Illegal
    | a == b && b == c = Equilateral
    | otherwise = let [x, y, z] = sort [a, b, c]
                  in if x + y <= z then Illegal
                     else if a == b || b == c || c == a then Isosceles
                     else Scalene
