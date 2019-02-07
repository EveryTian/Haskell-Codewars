-- https://www.codewars.com/kata/pascals-triangle

module Codewars.Kata.PascalsTriangle where
      
pascalsTriangle :: Int -> [Int]
pascalsTriangle 1 = [1]
pascalsTriangle 2 = [1, 1, 1]
pascalsTriangle n = addLine n $ pascalsTriangle (n - 1)
    where addLine :: Int -> [Int] -> [Int]
          addLine n old = let lastLine = drop (sum [1..n - 2]) old
                          in old ++ (1 : genNew lastLine ++ [1])
          genNew :: [Int] -> [Int]
          genNew [a, b] = [a + b]
          genNew (x:xs) = (x + head xs) : genNew xs
