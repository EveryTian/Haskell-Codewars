-- https://www.codewars.com/kata/directions-reduction

module Codewars.Kata.Reduction where

import Codewars.Kata.Reduction.Direction

-- data Direction = North | East | West | South deriving (Eq)

dirReduce :: [Direction] -> [Direction]
dirReduce = exec []
    where exec stack [] = reverse stack
          exec stack (North:xs) = if not (null stack) && head stack == South then exec (tail stack) xs else exec (North:stack) xs
          exec stack (South:xs) = if not (null stack) && head stack == North then exec (tail stack) xs else exec (South:stack) xs
          exec stack (East:xs)  = if not (null stack) && head stack == West  then exec (tail stack) xs else exec (East:stack) xs
          exec stack (West:xs)  = if not (null stack) && head stack == East  then exec (tail stack) xs else exec (West:stack) xs
