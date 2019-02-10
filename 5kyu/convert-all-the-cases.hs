-- http://www.codewars.com/kata/convert-all-the-cases

module ConvertCase where

import Data.Char (isLower, isUpper, toUpper, toLower)
import Data.List.Split (splitOn)

changeCase :: String -> String -> Maybe String
changeCase xs "snake" = snake $ anotherWords xs
changeCase xs "camel" = camel $ anotherWords xs
changeCase xs "kebab" = kebab $ anotherWords xs
changeCase xs _ = Nothing

anotherWords :: String -> [String]
anotherWords xs
    | isSnake xs = splitOn "_" xs
    | isCamel xs = camelWords xs "" []
    | isKebab xs = splitOn "-" xs
    | otherwise = []
          where camelWords "" s r = r ++ [s]
                camelWords (x:xs) s r
                    | isUpper x = camelWords xs [toLower x] (r ++ [s])
                    | otherwise = camelWords xs (s ++ [x]) r

isSnake :: String -> Bool
isSnake s = indexOf (\ x -> x == '-' || isUpper x) s == -1

isCamel :: String -> Bool
isCamel s = indexOf (\ x -> x == '_' || x == '-') s == -1

isKebab :: String -> Bool
isKebab s = indexOf (\ x -> isUpper x || x == '_') s == -1

snake :: [String] -> Maybe String
snake [] = Nothing
snake [x] = Just x
snake xs = Just $ foldl1 (\ x y -> x ++ ('_':y)) xs

camel :: [String] -> Maybe String
camel [] = Nothing
camel xs = Just $ foldl1 (\ x (y:ys) -> x ++ toUpper y : ys) xs

kebab :: [String] -> Maybe String
kebab [] = Nothing
kebab [x] = Just x
kebab xs = Just $ foldl1 (\ x y -> x ++ ('-':y)) xs

isNotLower :: Char -> Bool
isNotLower = not . isLower

indexOf :: (Char -> Bool) -> String -> Int
indexOf charFunc string = indexOf' charFunc string 0
    where indexOf' _ [] _ = -1
          indexOf' charFunc (x:xs) index = 
              if charFunc x then index
              else indexOf' charFunc xs (index + 1)
