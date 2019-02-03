-- https://www.codewars.com/kata/return-the-missing-element

module MissingElement where

getMissingElement :: [Int] -> Int
getMissingElement xs = head [i | i <- [0..9], i `notElem` xs]
