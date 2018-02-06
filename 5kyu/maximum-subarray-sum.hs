-- https://www.codewars.com/kata/maximum-subarray-sum

module MaxSequence where
import Data.List
-- Return the greatest subarray sum within the array of integers passed in.
maxSequence :: [Int] -> Int
maxSequence [] = 0
maxSequence x = if s < 0 then 0 else s
    where s = maximum $ map (maximum . (map sum) . init . tails . reverse) $ init . tails $ x
