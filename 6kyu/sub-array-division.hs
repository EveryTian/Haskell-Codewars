-- https://www.codewars.com/kata/sub-array-division

module SubArrayDiv where 

solve :: [Int] -> Int -> Bool
solve [] _ = False
solve xs n = hasTrue $ map (\ x -> x `mod` n == 0) $ f xs

hasTrue :: [Bool] -> Bool
hasTrue [] = False
hasTrue (x:xs) = if x then True else hasTrue xs

f :: [Int] -> [Int]
f arr = f' arr []
    where f' :: [Int] -> [Int] -> [Int]
          f' [] ret = ret
          f' (x:xs) ret = f' xs ((x:ret) ++ map (x +) ret)
