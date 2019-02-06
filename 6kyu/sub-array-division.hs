-- https://www.codewars.com/kata/sub-array-division

module SubArrayDiv where 

solve :: [Int] -> Int -> Bool
solve [] _ = False
solve xs n = any (\ x -> x `mod` n == 0) $ f xs

f :: [Int] -> [Int]
f arr = f' arr []
    where f' :: [Int] -> [Int] -> [Int]
--          f' [] ret = ret
--          f' (x:xs) ret = f' xs ((x:ret) ++ map (x +) ret)
          f' xs ret = foldl (\ ret x -> (x : ret) ++ map (x +) ret) ret xs
