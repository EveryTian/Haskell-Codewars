module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n s = f n s (length s)
    where f n s l
              | n > l = []
              | otherwise = map digitToInt (take n s) : f n (tail s) (l - 1)
