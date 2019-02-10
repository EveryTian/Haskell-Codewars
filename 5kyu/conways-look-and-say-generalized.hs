-- https://www.codewars.com/kata/conways-look-and-say-generalized

module LookAndSay where

import Data.List (group)

lookSay :: Integer -> Integer
lookSay = read . foldr1 (++) . map (\x -> show (length x) ++ [head x]) . group . show
