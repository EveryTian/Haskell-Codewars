-- https://www.codewars.com/kata/count-the-digit

module Codewars.G964.Countdig where

nbDig :: Int -> Int -> Int
nbDig n d = let d' = ['0'..'9'] !! d
                n' = map (show . (^2)) [0..n]
            in sum . map (length . filter (d' ==)) $ n'
