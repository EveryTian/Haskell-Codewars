module ArmstrongNumbers (armstrong) where

import Data.Char (ord)

armstrong :: (Show a, Integral a) => a -> Bool
armstrong x = fromIntegral (f (show x)) == fromIntegral x

f :: String -> Int
f numString = sum $ map (\ x -> (ord x - ord '0') ^ length numString) (reverse numString)
