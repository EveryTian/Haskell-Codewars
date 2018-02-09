-- https://www.codewars.com/kata/sort-arrays-ignoring-case

module Sort where

import Data.Char

sortme :: [String] -> [String]
sortme [] = []
sortme [x] = [x]
sortme (x:xs) = sortme [i | i <- xs, i `le` x] ++ [x] ++ sortme [i | i <- xs, i `gt` x]

cmp :: (String -> String -> Bool) -> String -> String -> Bool
cmp cmpFunc a b = let a' = strToLower a
                      b' = strToLower b
                   in a' `cmpFunc` b'

strToLower :: String -> String
strToLower = map toLower

le :: String -> String -> Bool
le = cmp (<=)

gt :: String -> String -> Bool
gt = cmp (>)
