-- http://www.codewars.com/kata/the-fusc-function-part-2

module Fusc where

fusc :: Integer -> Integer
fusc n = f n 1 0

-- f n a b = a * fusc n + b * fusc (n + 1)
f :: Integer -> Integer -> Integer -> Integer
f 0 a b = b
f 1 a b = a + b
{-
 - f (2 * k)     a b = (a + b) * fusc k +       b * fusc (k + 1)
 - f (2 * k + 1) a b =       a * fusc k + (a + b) * fusc (k + 1)
 -}
f n a b
    | even n    = let k = n `div` 2 in f k (a + b) b
    | otherwise = let k = n `div` 2 in f k a (a + b)
