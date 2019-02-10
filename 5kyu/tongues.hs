-- https://www.codewars.com/kata/tongues

module Codewars.Exercise.Tongues where

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

swapTable :: [(Char, Char)]
--                       abcdefghijklmnopqrstuvwxyz
swapTable = let table = "eplragfsoxvcwtibzdhnykmjuq"
            in zip (['a'..'z'] ++ ['A'..'Z']) (table ++ map toUpper table)

swapChar :: Char -> Char
-- swapChar c = case lookup c swapTable of
--     Nothing -> c
--     Just c' -> c'
swapChar c = fromMaybe c $ lookup c swapTable

tongues :: String -> String
tongues = map swapChar
