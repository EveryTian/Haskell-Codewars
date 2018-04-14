module Diamond (diamond) where

import Data.Char (chr, ord, isUpper)

diamond :: Char -> Maybe [String]
diamond 'A' = Just ["A"]
diamond c
  | not $ isUpper c = Nothing
  | otherwise =
      let n = ord c - ord 'A'
      in Just [f n i | i <- [0 .. n] ++ [n - 1, n - 2 .. 0]]
           where f n 0 =
                   let side = replicate n ' '
                   in side ++ ['A'] ++ side
                 f n i =
                   let side = replicate (n - i) ' '
                       mid = replicate (i * 2 - 1) ' '
                       ele = chr (i + ord 'A')
                   in side ++ [ele] ++ mid ++ [ele] ++ side
