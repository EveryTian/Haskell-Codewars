-- https://www.codewars.com/kata/playing-with-laziness

module Laziness where

type Matrix = [[Bool]]

findTrue :: Matrix -> (Int, Int)
findTrue = findTrue' [(0, 0)]
    where findTrue' :: [(Int, Int)] -> Matrix -> (Int, Int)
          findTrue' coords matrix =
              let coord = findCoord coords matrix
              in if fst coord == -1
                 then findTrue' (genNext coords) matrix
                 else coord
          findCoord :: [(Int, Int)] -> Matrix -> (Int, Int)
          findCoord [] matrix = (-1, -1)
          findCoord ((x, y):xs) matrix = if matrix !! x !! y
                                         then (x, y)
                                         else findCoord xs matrix
genNext :: [(Int, Int)] -> [(Int, Int)]
genNext arr = let x = getX arr + 1
              in (x, x):[(i, x) | i <- [0 .. x - 1]] ++ [(x, i) | i <- [0 .. x - 1]]
    where getX ((x, y):xs)
              | x == y = x
              | x /= y = getX xs

-- generate :: Int -> Int -> Matrix
-- generate n m =
--   let falses = repeat False
--       oneTrue = replicate m False ++ [ True ] ++ falses
--   in replicate n falses ++ [oneTrue] ++ repeat falses
