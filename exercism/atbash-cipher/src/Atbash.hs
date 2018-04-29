module Atbash (decode, encode) where

import Data.Char (toLower, isAlpha, isDigit, ord, chr)

decode :: String -> String
decode  = map codeChar . filter (\ c -> isAlpha c || isDigit c)

encode :: String -> String
encode  = unwords . f . map codeChar . filter (\ c -> isAlpha c || isDigit c)
    where f (a:b:c:d:e:xs) = [a, b, c, d, e] : f xs
          f [] = []
          f xs = [xs]

codeChar :: Char -> Char
codeChar c
    | isAlpha c = chr (25 - (ord (toLower c) - ord 'a') + ord 'a')
    | otherwise = c
