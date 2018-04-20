-- https://www.codewars.com/kata/bankers-plan

module Codewars.G964.Bankerplan where

fortune :: Int -> Double -> Int -> Int -> Double -> Bool
fortune f0 p c0 n i
    | f0 < 0 = False
    | n <= 1 = True
    | otherwise = fortune (truncate $ fromIntegral f0 * (1 + p / 100) - fromIntegral c0)
                          p
                          (truncate $ fromIntegral c0 * (1 + i / 100))
                          (n - 1)
                          i
