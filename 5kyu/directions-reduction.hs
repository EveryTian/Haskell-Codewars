-- https://www.codewars.com/kata/directions-reduction

module Codewars.Kata.Reduction where
import Codewars.Kata.Reduction.Direction

-- data Direction = North | East | West | South deriving (Eq)

dirReduce :: [Direction] -> [Direction]
dirReduce = exec []
    where exec stack [] = reverse stack
          exec stack (North:xs) = if length stack > 0 && (stack !! 0) == South then exec (drop 1 stack) xs else exec (North:stack) xs
          exec stack (South:xs) = if length stack > 0 && (stack !! 0) == North then exec (drop 1 stack) xs else exec (South:stack) xs
          exec stack (East:xs) = if length stack > 0 && (stack !! 0) == West then exec (drop 1 stack) xs else exec (East:stack) xs
          exec stack (West:xs) = if length stack > 0 && (stack !! 0) == East then exec (drop 1 stack) xs else exec (West:stack) xs