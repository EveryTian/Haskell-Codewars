module Triangle (rows) where

rows :: Int -> [[Integer]]
rows = reverse . rows'
    where rows' n
              | n <= 0 = []
              | n == 1 = [[1]]
              | otherwise = let row@(r0:_) = rows' (n - 1)
                            in (1 : nextRowTail r0) : row
          nextRowTail [x] = [1]
          nextRowTail (x0:x1:xs) = x0 + x1 : nextRowTail (x1:xs)
