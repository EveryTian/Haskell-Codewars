module Hamming (distance) where

import Data.List (nub, sort)

distance :: String -> String -> Maybe Int
distance xs ys
    | not (isDNA xs && isDNA ys) || length xs /= length ys = Nothing
    | otherwise = Just $ distance' xs ys
          where isDNA = all (`elem` "ACGT")
                distance' "" _ = 0
                distance' (x:xs) (y:ys)
                    | x /= y = 1 + distance' xs ys
                    | otherwise = distance' xs ys
