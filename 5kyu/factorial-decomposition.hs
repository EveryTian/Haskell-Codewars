-- https://www.codewars.com/kata/factorial-decomposition

module FactorialDecomposition.Kata (decomp) where

import Data.List (intercalate)

decomp :: Int -> String
decomp = strResList . zip primes . foldl zipWithPlus' [] . map (`dc` primes) . enumFromTo 2

strResList :: [(Int, Int)] -> String
strResList = intercalate " * " . map strRes
    where strRes :: (Int, Int) -> String
          strRes (b, 1) = show b
          strRes (b, p) = show b ++ '^' : show p

dc :: Int -> [Int] -> [Int]
dc 1 _ = []
dc n (x:xs) = let (t, r) = n `divTimes` x in t : dc r xs

divTimes :: Int -> Int -> (Int, Int)
n `divTimes` m
    | n `rem` m == 0 = let (t, r) = divTimes (n `div` m) m in (t + 1, r)
    | otherwise = (0, n)

zipWithPlus' :: [Int] -> [Int] -> [Int]
zipWithPlus' xs [] = xs
zipWithPlus' [] ys = ys
zipWithPlus' (x:xs) (y:ys) = (x + y) : zipWithPlus' xs ys

primes :: [Int]
primes = 2 : 3 : filter (\ n -> all ((0 /=) . mod n) $ takeWhile (\ i -> i * i <= n) primes) [5, 7 ..]
