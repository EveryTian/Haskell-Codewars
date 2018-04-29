module CryptoSquare (encode) where

import Data.Char (isAlpha, isDigit, toLower)

encode :: String -> String
encode xs = let s = preDeal xs
                (r, c) = getRC s
                sl = sep s c
            in reGen sl

reGen :: [String] -> String
reGen [] = []
reGen xs
    | null $ head xs = []
    | otherwise = map head (filter (not . null) xs) ++ ' ' : reGen (map anotherTail xs)
    where anotherTail [] = []
          anotherTail (_:t) = t

getRC :: String -> (Int, Int)
getRC xs = let len = length xs
           in f len 0 0
    where f len r c
              | r * c >= len = (r, c)
              | r == c = f len r (c + 1)
              | otherwise = f len (r + 1) c

preDeal :: String -> String
preDeal = map toLower . filter (\ x -> isAlpha x || isDigit x)

sep :: String -> Int -> [String]
sep s c = f s c (length s)
    where f s c len
              | len <= c = [s]
              | otherwise = take c s : f (drop c s) c (len - c)
