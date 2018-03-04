-- https://www.codewars.com/kata/your-order-please

module Codewars.Kata.YourOrderPlease where

import Data.List (sortBy)
import Data.Char (isDigit)

yourOrderPlease :: String -> String
yourOrderPlease = unwords . sortBy f . words
    where f a b = let a' = (filter isDigit a) !! 0
                      b' = (filter isDigit b) !! 0
                  in compare a' b'
