-- https://www.codewars.com/kata/thinking-and-testing-incomplete-string

module Codewars.ThinkingAndTesting02 where

import Data.Char (ord, chr)

testit :: String -> String
testit "" = ""
testit [c] = [c]
testit (c0:c1:cs) = chr ((ord c0 + ord c1) `div` 2) : testit cs
