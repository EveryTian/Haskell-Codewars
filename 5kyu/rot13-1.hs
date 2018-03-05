-- https://www.codewars.com/kata/rot13-1

module Rot13 where

import Data.Char

rot13 :: String -> String
rot13 = let f :: Char -> Char
            f c
              | isAsciiUpper c = let c' = chr (ord c + 13)
                            in if c' > 'Z'
                               then chr (ord c' - 26) else c'
              | isAsciiLower c = let c' = chr (ord c + 13)
                            in if c' > 'z'
                               then chr (ord c' - 26) else c'
              | otherwise = c
        in map f
