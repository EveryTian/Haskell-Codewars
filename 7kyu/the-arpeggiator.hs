-- https://www.codewars.com/kata/the-arpeggiator

module Codewars.Kata.Arpeggio where

scale = "ABCDEFGABCDEFG" :: String

arpeggio :: Char -> Maybe String
arpeggio x
    | x `notElem` scale = Nothing
    | otherwise = let i = x `idx` scale
                  in Just [x, scale !! (i + 2), scale !! (i + 4), x]

idx :: Eq a => a -> [a] -> Int
idx = idx' 0
    where idx' i a [] = -1
          idx' i a (x:xs)
              | a == x = i
              | a /= x = idx' (i + 1) a xs
