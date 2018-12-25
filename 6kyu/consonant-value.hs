-- https://www.codewars.com/kata/consonant-value

module ConsonantVal where 

import Data.Char (ord)
import Data.List.Split (splitOneOf)

solve :: String -> Int
solve = maximum . map (sum . map charToInt) . splitOneOf "aeiou"
    where charToInt c = ord c - ord 'a' + 1

-- Misunderstood
-- solve :: String -> Int
-- solve = maximum . map sum . flip sepConsList [] . map charToInt
--     where charToInt c = ord c - ord 'a' + 1
--           sepConsList :: [Int] -> [[Int]] -> [[Int]]
--           sepConsList [] res = res
--           sepConsList (x:xs) [] = sepConsList xs [[x]]
--           sepConsList (x:xs) rrr@(rr@(r:rs):rss)
--               | x >= r = sepConsList xs ((x:rr):rss)
--               | otherwise = sepConsList xs ([x]:rrr)
