-- https://www.codewars.com/kata/return-a-sorted-list-of-objects

module Sorted where

import Data.List (sortBy)
import Data.Function (on)

sortList :: Ord b => (a -> b) -> [a] -> [a]
-- sortList f = sortBy (\ a b -> compare (f a) (f b))
sortList f = sortBy (compare `on` f)
