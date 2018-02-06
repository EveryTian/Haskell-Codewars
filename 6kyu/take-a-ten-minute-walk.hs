-- https://www.codewars.com/kata/take-a-ten-minute-walk

module Codewars.Kata.TenMinuteWalk where
import Data.List
isValidWalk :: [Char] -> Bool
isValidWalk walk = 
  if length (take 11 walk) /= 10 then False
  else let charNum c = length $ filter (== c) walk
       in if charNum 'n' == charNum 's' && charNum 'e' == charNum 'w'
          then True else False
