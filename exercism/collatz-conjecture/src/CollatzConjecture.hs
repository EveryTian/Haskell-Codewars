module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz x
  | x <= 0 = Nothing
  | otherwise = collatz' x 0
      where collatz' :: Integer -> Integer -> Maybe Integer
            collatz' 1 r = Just r
            collatz' x r
              | even x = collatz' (x `div` 2) (r + 1)
              | otherwise = collatz' (3 * x + 1) (r + 1)
