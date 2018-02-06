-- https://www.codewars.com/kata/filter-coffee

module Codewars.G964.FilterCoffee where
import Data.List
search :: Int -> [Int] -> String
search budget prices = let l = show $ filter (<= budget) $ sort prices
                       in drop 1 $ take (length l - 1) l
