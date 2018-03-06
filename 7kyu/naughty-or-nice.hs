-- https://www.codewars.com/kata/naughty-or-nice

module NaughtyNice where

type Warrior = (String, Bool)

getNiceNames :: [Warrior] -> [String]
getNiceNames = map (\x -> fst x) . filter (\x -> snd x)

getNaughtyNames :: [Warrior] -> [String]
getNaughtyNames = map (\x -> fst x) . filter (\x -> not $ snd x)
