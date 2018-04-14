module Raindrops (convert) where

convert :: Int -> String
convert n = let s = f n 7 "Plong" . f n 5 "Plang" . f n 3 "Pling" $ ""
            in if null s then show n else s
    where f n x a s = if n `mod` x == 0 then s ++ a else s