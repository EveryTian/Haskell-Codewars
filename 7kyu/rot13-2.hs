-- https://www.codewars.com/kata/rot13-2

module ROT13 where

import Data.Char (chr, ord)

rot13 :: String -> String
rot13 = let f :: Char -> Char
            f c
              | elem c ['a'..'z'] = let c' = chr (ord c + 13)
                                    in if c' > 'z' then chr (ord c' - 26) else c'
              | otherwise = c
        in map f
