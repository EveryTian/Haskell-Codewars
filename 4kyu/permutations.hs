-- https://www.codewars.com/kata/permutations

module Codewars.Kata.Permutations (permutations) where
import Data.List hiding (permutations)

permutations :: String -> [String]
permutations = delSame . allOrder
    where allOrder "" = [""]
          allOrder (c:"") = [[c]]
          allOrder str = foldl (++) [] [[[str !! j] ++ i | i <- allOrder $ removeAt j str] | j <- [0 .. length str - 1]]
          removeAt n str = take n str ++ drop (n + 1) str
          delSame x = [i !! 0 | i <- group . sort $ x, length i > 0]
