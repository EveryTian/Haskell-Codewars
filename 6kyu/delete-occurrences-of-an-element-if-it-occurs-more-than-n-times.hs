-- https://www.codewars.com/kata/delete-occurrences-of-an-element-if-it-occurs-more-than-n-times

module Codewars.Kata.Deletion where

type KeyValuePair k v = (k, v)
getValue :: (Eq k) => [KeyValuePair k v] -> k -> Maybe v
getValue [] key = Nothing
getValue ((k, v):xs) key
    | key == k = Just v
    | otherwise = getValue xs key
accValue :: (Eq k, Num v) => [KeyValuePair k v] -> k -> [KeyValuePair k v]
accValue [] key = [(key, 1)]
accValue ((k, v):xs) key
    | key == k = (k, v + 1):xs
    | otherwise = (k, v):(accValue xs key)

deleteNth :: [Int] -> Int -> [Int]
deleteNth = deleteNth' []
    where deleteNth' :: [KeyValuePair Int Int] -> [Int] -> Int -> [Int]
          deleteNth' statistics [] n = []
          deleteNth' statistics (x:xs) n
              | getValue statistics x == Just n = deleteNth' statistics xs n
              | otherwise = x:(deleteNth' (accValue statistics x) xs n)
