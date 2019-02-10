-- https://www.codewars.com/kata/simple-fun-number-329-best-score

module BestScore.Kata (bestScore) where

bestScore :: [Int] -> [Int] -> Int
bestScore [] [] = 0
bestScore a b
    | null a = (-2) * length b
    | null b = 3 * length a
    | otherwise = maximum [scoreDiff x a b | x <- (0:1:a) ++ b ++ [maximum b + 1]] 

score :: Int -> [Int] -> Int
score line team = sum [if i < line then 2 else 3 | i <- team]

scoreDiff :: Int -> [Int] -> [Int] -> Int
scoreDiff line a b = score line a - score line b
