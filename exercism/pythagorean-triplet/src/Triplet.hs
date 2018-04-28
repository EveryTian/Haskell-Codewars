module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

import Data.List (sort, nub)

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c) = a * a + b * b == c * c

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c = let arr = sort [a, b, c]
                  in (head arr, arr !! 1, arr !! 2)

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets = ((nub . sort) .) . f
    where f minBound maxBound
              | minBound > maxBound = []
              | otherwise = concatMap (findTri minBound) [minBound..maxBound] ++ f (minBound + 1) maxBound
              where findTri a b = 
                        let t1 = truncate . sqrt . fromIntegral $ a * a + b * b
                            t2 = truncate . sqrt . fromIntegral $ b * b - a * a
                        in [mkTriplet a b t1 | minBound <= t1 && t1 <= maxBound && t1 * t1 == a * a + b * b]
                        ++ [mkTriplet a b t2 | minBound <= t2 && t2 <= maxBound && t2 * t2 == b * b - a * a]
