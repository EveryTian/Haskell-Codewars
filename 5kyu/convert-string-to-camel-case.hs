-- https://www.codewars.com/kata/convert-string-to-camel-case

module CamelCase where
import Data.Char
toCamelCase :: String -> String
toCamelCase str = fun False str ""
    where fun _ [] ret = ret
          fun afterSpace (x:xs) ret = 
              if x == '_' || x == '-'
              then fun True xs ret
              else fun False xs $ ret ++ [if afterSpace then toUpper x else x]
