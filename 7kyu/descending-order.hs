-- https://www.codewars.com/kata/descending-order

module DescendingOrder where

import Data.List (sortBy)

descendingOrder :: Integer -> Integer
-- descendingOrder = read . reverse . sort . show
descendingOrder = read . sortBy (flip compare) . show
