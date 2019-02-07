-- https://www.codewars.com/kata/permutations

module Codewars.Kata.Permutations (permutations) where

import Data.List hiding (permutations)

permutations :: String -> [String]
permutations = delSame . allOrder
    where allOrder "" = [""]
          allOrder (c:"") = [[c]]
          allOrder str = concat [[(str !! j) : i | i <- allOrder $ removeAt j str] | j <- [0 .. length str - 1]]
          removeAt n str = take n str ++ drop (n + 1) str
          delSame x = [head i | i <- group . sort $ x, not $ null i]
