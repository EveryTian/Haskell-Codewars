module Prime (nth) where

nth :: Int -> Maybe Integer
nth n = if n <= 0 then Nothing else
    let primes = 2 : 3 : filter (\ n -> all ((0 /=) . mod n) $ takeWhile (\ i -> i * i <= n) primes) [5, 7 ..]
    in Just $ primes !! (n - 1)
