-- https://www.codewars.com/kata/simple-string-reversal-ii

module SimpleStrRev where 

solve :: String -> Int -> Int -> String
solve xs a b = let part1 = take a xs
                   part2 = drop a $ take (b + 1) xs
                   part3 = drop (b + 1) xs
               in part1 ++ reverse part2 ++ part3
