-- https://www.codewars.com/kata/get-the-middle-character

module Codewars.G964.Getmiddle where

getMiddle :: String -> String
getMiddle [x] = [x]
getMiddle [x, y] = [x, y]
getMiddle (x:xs) = getMiddle $ take (length xs - 1) xs
