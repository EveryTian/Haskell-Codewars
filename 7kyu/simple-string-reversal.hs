-- https://www.codewars.com/kata/simple-string-reversal

module SimpleReversal where 

solve :: String -> String
solve ys = let numList = map length $ words ys
               string = reverse $ filter (\ x -> x /= ' ') ys
           in reverse $ f string numList ""
           where f "" _ r = r
                 f _ [] r = r
                 f s (0:xs) r = f s xs (' ':r)
                 f (s0:s) (x:xs) r = f s ((x - 1):xs) (s0:r)
