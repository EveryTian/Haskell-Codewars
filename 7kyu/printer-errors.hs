-- https://www.codewars.com/kata/printer-errors

module Codewars.G964.Printer where

import Data.Char (ord)

printerError :: String -> String
printerError b = show (errorNum b) ++ "/" ++ show (length b)
    where errorNum b = sum [1 | i <- b, ord i > ord 'm']
