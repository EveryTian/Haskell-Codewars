-- https://www.codewars.com/kata/consecutive-strings

module Codewars.G964.Longestconsec where

longestConsec :: [String] -> Int -> String
longestConsec strarr k
    | null strarr = ""
    | length strarr < k = ""
    | k <= 0 = ""
    | otherwise = foldr ((\ x longest -> if length x >= length longest then x else longest) . concat) "" $ getAllConsecArr k strarr
        where takeElem arr n = if length arr > n then arr !! n else ""
              getConsecArr start num arr
                  | num == 0 = []
                  | otherwise = arr `takeElem` start : getConsecArr (start + 1) (num -1) arr
              getAllConsecArr num arr = [getConsecArr i num arr | i <- [0 .. length arr - 1]]
