-- https://www.codewars.com/kata/find-the-missing-term-in-an-arithmetic-progression

module Codewars.Kata.Arithmetic where

findMissing :: Integral n => [n] -> n
findMissing arr@(x0:x1:xs) = let diff = getDiff arr
    in if diff == 0 then x0
       else findMissing' arr diff
    where getDiff = getDiff' 0
          getDiff' 0 (a0:a1:xs)
              | a0 == a1 = 0
              | otherwise = getDiff' (a1 - a0) (a1:xs)
          getDiff' diff (a0:a1:xs)
              | (a1 - a0 == diff) = diff
              | (a1 - a0 == 2 * diff) = diff
              | (a1 - a0 == diff `div` 2) = a1 - a0
          findMissing' (x0:x1:xs) diff
              | x1 - x0 == 2 * diff = x0 + diff
              | otherwise = findMissing' (x1:xs) diff
