-- https://www.codewars.com/kata/scramblies

module Codewars.G964.Scramblies where

import Data.List (sort, group)

scramble :: [Char] -> [Char] -> Bool
scramble s1 s2 =
    let f :: String -> [(Char, Int)]
        f = map (\x -> (x !! 0, length x)) . group . sort
        s1' = f s1
        s2' = f s2
    in test s1' s2'
    where test :: [(Char, Int)] -> [(Char, Int)] -> Bool
          test _ [] = True
          test a1 ((c, n):as)
              | get a1 c >= n = test a1 as
              | otherwise = False
          get :: [(Char, Int)] -> Char -> Int
          get [] _ = 0
          get ((c, n):xs) ch
              | c == ch = n
              | c /= ch = get xs ch
