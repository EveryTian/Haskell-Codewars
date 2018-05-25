-- https://www.codewars.com/kata/josephus-survivor

module Codewars.G964.Josephus where

josephusSurvivor :: Int -> Int -> Int
josephusSurvivor 1 _ = 1
josephusSurvivor n k = case josephusOnce [1..n] k
                       of [x] -> x
                          arr -> j arr k
    where j arr k = case josephusOnce arr k
                    of [x] -> x
                       arr' -> j arr' k
          josephusOnce :: [Int] -> Int -> [Int]
          josephusOnce arr k = case k `mod` length arr
                               of 0 -> init arr
                                  k' -> drop k' arr ++ take (k' - 1) arr
