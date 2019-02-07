-- https://www.codewars.com/kata/make-a-spiral

module Spiral where

spiralize :: Int -> [[Int]]
spiralize n
    | n `mod` 4 == 2 = spiralize' 6 [[1,1],[0,1]] n
    | n `mod` 4 == 1 = spiralize' 5 [[1]] n
    | n `mod` 4 == 3 = spiralize' 7 [[1,1,1],[0,0,1],[1,1,1]] n
    | otherwise = spiralize' 4 [] n
spiralize' cur mat n
    | cur > n = mat
    | otherwise = spiralize' (cur + 4)
                             ([replicate cur 1] ++
                              [replicate (cur - 1) 0 ++ [1]] ++
                              [(if i == 0 then [1, 1] else [1, 0]) ++
                               (mat !! i) ++ [0, 1] | i <- [0..length mat - 1]] ++
                              [1 : replicate (cur - 2) 0 ++ [1]] ++
                              [replicate cur 1])
                             n
