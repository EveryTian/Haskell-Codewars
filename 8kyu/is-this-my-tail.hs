-- https://www.codewars.com/kata/is-this-my-tail

module Codewars.IsThisMyTail where

correctTail :: String -> Char -> Bool
correctTail bod t = last bod == t
