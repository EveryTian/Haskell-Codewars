-- https://www.codewars.com/kata/reverse-words

module Reverse where

reverseWords :: String -> String
reverseWords "" = ""
reverseWords x = foldl1 (++) . map reverse . newWords $ x

words' :: [String] -> String -> String -> [String]
words' ret "" "" = ret
words' ret temp "" = words' (ret ++ [temp]) "" ""
words' ret "" (x:xs) = words' ret [x] xs
words' ret temp (x:xs)
    | head temp == ' ' && x == ' ' || head temp /= ' ' && x /= ' ' = words' ret (temp ++ [x]) xs
    | otherwise = words' (ret ++ [temp]) [x] xs

newWords :: String -> [String]
newWords = words' [] ""
