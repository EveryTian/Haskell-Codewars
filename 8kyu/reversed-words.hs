-- https://www.codewars.com/kata/reversed-words

module ReverseWords where

import Data.List

reverseWords :: String -> String
reverseWords s
    | null (words s) = s
    | otherwise = foldr1 (\ y x -> x ++ ' ':y) $ words s
