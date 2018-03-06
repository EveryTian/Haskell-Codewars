-- https://www.codewars.com/kata/alphabet-symmetry

module AlphabetSymm where

import Data.Char (toLower, ord)

solve :: [String] -> [Int]
solve = map $ solveOne 1 . map toLower
    where solveOne :: Int -> String -> Int
          solveOne _ "" = 0
          solveOne index (x:xs)
              | index == ord x - ord 'a' + 1 = 1 + solveOne (index + 1) xs
              | otherwise = solveOne (index + 1) xs
