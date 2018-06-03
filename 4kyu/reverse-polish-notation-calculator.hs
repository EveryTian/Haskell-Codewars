-- https://www.codewars.com/kata/reverse-polish-notation-calculator

module RPN where

calc :: String -> Double
calc str = case words str of
    [] -> 0
    wordsList -> head . foldl f [] $ wordsList
    where f :: [Double] -> String -> [Double]
          f (x:y:ss) "+" = (x + y) : ss
          f (x:y:ss) "-" = (y - x) : ss
          f (x:y:ss) "*" = (x * y) : ss
          f (x:y:ss) "/" = (y / x) : ss
          f s sn = read sn : s
