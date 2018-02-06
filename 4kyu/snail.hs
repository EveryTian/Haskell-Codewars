-- https://www.codewars.com/kata/snail

module Snail where

snail :: [[Int]] -> [Int]
snail arr = snail' (length arr) arr
    where snail' 0 arr = []
          snail' 1 arr = arr !! 0
          snail' n arr = (arr !! 0) ++ 
                         [i !! (n - 1) | i <- take (n - 2) . drop 1 $ arr] ++
                         (reverse $ arr !! (n - 1)) ++
                         (reverse [i !! 0 | i <- take (n - 2) . drop 1 $ arr]) ++
                         (snail' (n - 2)
                                 (let f = take (n - 2) . drop 1 
                                  in [f i | i <- f $ arr]))
