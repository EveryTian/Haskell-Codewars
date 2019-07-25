-- https://www.codewars.com/kata/get-ascii-value-of-character

module ASCII where

import Data.Char (ord)

getASCII :: Char -> Int
getASCII = ord
