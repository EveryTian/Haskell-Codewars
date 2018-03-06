-- https://www.codewars.com/kata/power-of-two

module Codewars.Kata.PowerOfTwo where

import Data.Bits (popCount)

isPowerOfTwo :: Int -> Bool
isPowerOfTwo = (1 ==) . popCount
