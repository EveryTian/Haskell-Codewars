module Luhn (isValid) where

import Data.Char (digitToInt)

isValid :: String -> Bool
isValid n = case filter (/= ' ') n of
    "" -> False
    [_] -> False
    s -> fstCheck s && luhnCheck s
    where fstCheck = not . any (`notElem` ['0'..'9'])
          luhnCheck = (== 0) . (`mod` 10) . luhn' . reverse . map digitToInt
          luhn' [] = 0
          luhn' [x] = x
          luhn' (x0:x1:xs) = x0 + x1 * 2 - (if x1 >= 5 then 9 else 0) + luhn' xs
