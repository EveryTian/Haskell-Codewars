-- https://www.codewars.com/kata/take-a-ten-minute-walk

module Codewars.Kata.TenMinuteWalk where

import Data.List

isValidWalk :: String -> Bool
isValidWalk walk =
  let charNum c = length $ filter (== c) walk
  in length (take 11 walk) == 10 && charNum 'n' == charNum 's' && charNum 'e' == charNum 'w'
