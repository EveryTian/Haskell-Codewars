module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n 
    | n <= 0 = Nothing 
    | otherwise = 
          let s = sum [i| i <- [1 .. n - 1], n `mod` i == 0]
          in Just (if s == n then Perfect
                   else if s < n then Deficient
                   else Abundant)
