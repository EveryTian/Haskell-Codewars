-- https://www.codewars.com/kata/snail

module Snail where

snail :: [[Int]] -> [Int]
snail arr = snail' (length arr) arr
    where snail' 0 arr = []
          snail' 1 arr = head arr
          snail' n arr = head arr ++ 
                         [i !! (n - 1) | i <- take (n - 2) . tail $ arr] ++
                         reverse (arr !! (n - 1)) ++
                         reverse [head i | i <- take (n - 2) . tail $ arr] ++
                         snail' (n - 2)
                                (let f = take (n - 2) . drop 1 
                                 in [f i | i <- f arr])
