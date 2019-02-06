-- https://www.codewars.com/kata/sort-the-odd

module SortArray where

import Data.List (sort)

sortArray :: [Int] -> [Int]
sortArray xs = let oddArr = sort $ filter odd xs
                   evenNumWithPosArr = getEvenNumWithPosArr 0 xs
               in merge oddArr evenNumWithPosArr 0
               where getEvenNumWithPosArr :: Int -> [Int] -> [(Int, Int)] 
                     getEvenNumWithPosArr _ [] = []
                     getEvenNumWithPosArr curPos (x:xs)
                         | even x = (x, curPos) : getEvenNumWithPosArr (curPos + 1) xs
                         | otherwise = getEvenNumWithPosArr (curPos + 1) xs
                     merge :: [Int] -> [(Int, Int)] -> Int -> [Int]
                     merge [] evenNumWithPosArr@(y:ys) _ = map fst evenNumWithPosArr
                     merge oddArr@(x:xs) [] _ = oddArr
                     merge [] [] _ = []
                     merge oddArr@(x:xs) evenNumWithPosArr@(y:ys) curPos
                         | snd y == curPos = fst y : merge oddArr ys (curPos + 1)
                         | otherwise = x : merge xs evenNumWithPosArr (curPos + 1)
