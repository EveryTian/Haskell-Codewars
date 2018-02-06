-- https://www.codewars.com/kata/growth-of-a-population

module Codewars.G964.Arge where

nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p = yearData 0 p0 percent aug p
yearData :: Int -> Int -> Double -> Int -> Int -> Int
yearData x px percent aug p = 
  if px >= p then x 
  else yearData (x + 1) 
                (fromIntegral $ truncate $ fromIntegral px + fromIntegral aug + fromIntegral px * percent / 100)
                percent
                aug
                p