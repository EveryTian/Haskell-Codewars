-- https://www.codewars.com/kata/your-order-please

module Codewars.Kata.YourOrderPlease where

import Data.List (sortBy)
import Data.Char (isDigit)

yourOrderPlease :: String -> String
yourOrderPlease = unwords . sortBy f . words
    where f a b = let a' = head (filter isDigit a)
                      b' = head (filter isDigit b)
                  in compare a' b'
